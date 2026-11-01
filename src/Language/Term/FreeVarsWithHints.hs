module Language.Term.FreeVarsWithHints (freeTermVarsWithHints) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.VarKind qualified as VK
import Language.Term.Term qualified as TM
import Logger.Hint

freeTermVarsWithHints :: TM.Term -> S.Set (Hint, Ident)
freeTermVarsWithHints term =
  case term of
    m :< TM.Var x ->
      S.singleton (m, x)
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.PiIntro k impArgs expArgs defaultArgs e -> do
      let body = removeBinders (catMaybes [AttrL.fromAttr k]) (freeTermVarsWithHints e)
      removeBinders (impArgs ++ expArgs) (collectDefaultArgs defaultArgs body)
    _ :< TM.PiElim _ _ e _ expArgs defaultArgs -> do
      let xs = freeTermVarsWithHints e
      let ys1 = S.unions $ map freeTermVarsWithHints expArgs
      let ys2 = S.unions $ map freeTermVarsWithHints (catMaybes defaultArgs)
      S.unions [xs, ys1, ys2]
    _ :< TM.DataIntro _ _ _ _ consArgs ->
      S.unions $ map freeTermVarsWithHints consArgs
    m :< TM.DataElim _ _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeTermVarsWithHints es
      let binder = zipWith (\o t -> (m, VK.normal, o, t)) os ts
      let xs2 = removeBinders binder (collectDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< TM.BoxIntro _ letSeq e -> do
      let (xts, es) = unzip letSeq
      removeBinders xts (S.unions $ map freeTermVarsWithHints (e : es))
    _ :< TM.BoxIntroLift _ e ->
      freeTermVarsWithHints e
    _ :< TM.EmbedIntro e ->
      freeTermVarsWithHints e
    _ :< TM.BoxElim _ castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      removeBinders xts (S.unions $ map freeTermVarsWithHints $ es ++ [e2])
    _ :< TM.CodeIntro _ e ->
      freeTermVarsWithHints e
    _ :< TM.CodeElim _ e ->
      freeTermVarsWithHints e
    _ :< TM.TauIntro _ ->
      S.empty
    _ :< TM.TauElim _ _ e1 e2 ->
      S.union (freeTermVarsWithHints e1) (freeTermVarsWithHints e2)
    _ :< TM.Let mxt e1 e2 -> do
      let set1 = freeTermVarsWithHints e1
      let set2 = removeBinders [mxt] (freeTermVarsWithHints e2)
      S.union set1 set2
    _ :< TM.Invoke _ body ->
      freeTermVarsWithHints body
    _ :< TM.Prim _ ->
      S.empty
    _ :< TM.Magic _ der ->
      collectMagic der

collectDefaultArgs :: [(BinderF TM.Type, TM.Term)] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
collectDefaultArgs defaultArgs zs =
  case defaultArgs of
    [] ->
      zs
    (binder, value) : rest ->
      S.union (freeTermVarsWithHints value) (removeBinders [binder] (collectDefaultArgs rest zs))

removeBinders :: [BinderF TM.Type] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
removeBinders binder zs =
  case binder of
    [] ->
      zs
    ((_, _, x, _) : xts) ->
      S.filter (\(_, y) -> y /= x) $ removeBinders xts zs

collectDecisionTree :: DT.DecisionTree TM.Type TM.Term -> S.Set (Hint, Ident)
collectDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeTermVarsWithHints (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch _ caseList ->
      collectCaseList caseList

collectCaseList :: DT.CaseList TM.Type TM.Term -> S.Set (Hint, Ident)
collectCaseList (fallbackClause, clauseList) = do
  let xs1 = collectDecisionTree fallbackClause
  let xs2 = S.unions $ map collectCase clauseList
  S.union xs1 xs2

collectCase :: DT.Case TM.Type TM.Term -> S.Set (Hint, Ident)
collectCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      collectDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {consImpArgs, consArgs, cont}) -> do
      removeBinders (consImpArgs ++ consArgs) (collectDecisionTree cont)

collectMagic :: M.Magic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
collectMagic magic =
  case magic of
    M.LowMagic lowMagic ->
      collectLowMagic lowMagic
    M.Calloc _ num size ->
      S.union (freeTermVarsWithHints num) (freeTermVarsWithHints size)
    M.Malloc _ size ->
      freeTermVarsWithHints size
    M.MallocOpaque _ size ->
      freeTermVarsWithHints size
    M.Realloc _ ptr size ->
      S.union (freeTermVarsWithHints ptr) (freeTermVarsWithHints size)
    M.Free _ ptr ->
      freeTermVarsWithHints ptr
    M.InspectType {} ->
      S.empty
    M.EqType {} ->
      S.empty
    M.ShowType _ ->
      S.empty
    M.TextCons rune text ->
      S.union (freeTermVarsWithHints rune) (freeTermVarsWithHints text)
    M.TextUncons _ text ->
      freeTermVarsWithHints text
    M.MakeSwitch _ key fallback clauses ->
      S.unions $ map freeTermVarsWithHints [key, fallback, clauses]
    M.FreeVariables _ code ->
      freeTermVarsWithHints code
    M.CompileError msg ->
      freeTermVarsWithHints msg
    M.GetOriginFileName ->
      S.empty
    M.GetOriginLine ->
      S.empty
    M.GetOriginColumn ->
      S.empty

collectLowMagic :: LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
collectLowMagic lowMagic =
  case lowMagic of
    LM.Cast _ _ value ->
      freeTermVarsWithHints value
    LM.Store _ _ value pointer ->
      S.union (freeTermVarsWithHints value) (freeTermVarsWithHints pointer)
    LM.Load _ pointer ->
      freeTermVarsWithHints pointer
    LM.Shift pointer offset ->
      S.union (freeTermVarsWithHints pointer) (freeTermVarsWithHints offset)
    LM.Alloca _ size ->
      freeTermVarsWithHints size
    LM.External _ _ _ args varArgs -> do
      let argVars = S.unions $ map freeTermVarsWithHints args
      let varArgVars = S.unions $ map (freeTermVarsWithHints . fst) varArgs
      S.union argVars varArgVars
    LM.Global {} ->
      S.empty
    LM.OpaqueValue e ->
      freeTermVarsWithHints e
    LM.CallType func arg1 arg2 arg3 ->
      S.unions $ map freeTermVarsWithHints [func, arg1, arg2, arg3]
