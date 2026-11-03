module Language.Term.FreeVarsWithHints (freeTermVarsWithHints) where

import Control.Comonad.Cofree
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
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

freeTermVarsWithHints :: TM.Term -> [(Hint, Ident)]
freeTermVarsWithHints =
  nubOrdOn snd . collect

collect :: TM.Term -> [(Hint, Ident)]
collect term =
  case term of
    m :< TM.Var x ->
      [(m, x)]
    _ :< TM.VarGlobal {} ->
      []
    _ :< TM.PiIntro k impArgs expArgs defaultArgs e -> do
      let body = removeBinders (catMaybes [AttrL.fromAttr k]) (collect e)
      removeBinders (impArgs ++ expArgs) (collectDefaultArgs defaultArgs body)
    _ :< TM.PiElim _ _ e _ expArgs defaultArgs -> do
      let xs = collect e
      let ys1 = concatMap collect expArgs
      let ys2 = concatMap collect (catMaybes defaultArgs)
      xs ++ ys1 ++ ys2
    _ :< TM.DataIntro _ _ _ _ consArgs ->
      concatMap collect consArgs
    m :< TM.DataElim _ _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = concatMap collect es
      let binder = zipWith (\o t -> (m, VK.normal, o, t)) os ts
      let xs2 = removeBinders binder (collectDecisionTree decisionTree)
      xs1 ++ xs2
    _ :< TM.BoxIntro _ letSeq e -> do
      let (xts, es) = unzip letSeq
      removeBinders xts (concatMap collect (e : es))
    _ :< TM.BoxIntroLift _ e ->
      collect e
    _ :< TM.EmbedIntro e ->
      collect e
    _ :< TM.BoxElim _ castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      removeBinders xts (concatMap collect $ es ++ [e2])
    _ :< TM.CodeIntro _ e ->
      collect e
    _ :< TM.CodeElim _ e ->
      collect e
    _ :< TM.TauIntro _ ->
      []
    _ :< TM.TauElim _ _ e1 e2 ->
      collect e1 ++ collect e2
    _ :< TM.Let mxt e1 e2 -> do
      let set1 = collect e1
      let set2 = removeBinders [mxt] (collect e2)
      set1 ++ set2
    _ :< TM.Invoke _ body ->
      collect body
    _ :< TM.Prim _ ->
      []
    _ :< TM.Magic _ der ->
      collectMagic der

collectDefaultArgs :: [(BinderF TM.Type, TM.Term)] -> [(Hint, Ident)] -> [(Hint, Ident)]
collectDefaultArgs defaultArgs zs =
  case defaultArgs of
    [] ->
      zs
    (binder, value) : rest ->
      collect value ++ removeBinders [binder] (collectDefaultArgs rest zs)

removeBinders :: [BinderF TM.Type] -> [(Hint, Ident)] -> [(Hint, Ident)]
removeBinders binder zs =
  case binder of
    [] ->
      zs
    ((_, _, x, _) : xts) ->
      filter (\(_, y) -> y /= x) $ removeBinders xts zs

collectDecisionTree :: DT.DecisionTree TM.Type TM.Term -> [(Hint, Ident)]
collectDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      collect (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      []
    DT.Switch _ caseList ->
      collectCaseList caseList

collectCaseList :: DT.CaseList TM.Type TM.Term -> [(Hint, Ident)]
collectCaseList (fallbackClause, clauseList) = do
  let xs1 = concatMap collectCase clauseList
  let xs2 = collectDecisionTree fallbackClause
  xs1 ++ xs2

collectCase :: DT.Case TM.Type TM.Term -> [(Hint, Ident)]
collectCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      collectDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {consImpArgs, consArgs, cont}) -> do
      removeBinders (consImpArgs ++ consArgs) (collectDecisionTree cont)

collectMagic :: M.Magic BLT.BaseLowType TM.Type TM.Term -> [(Hint, Ident)]
collectMagic magic =
  case magic of
    M.LowMagic lowMagic ->
      collectLowMagic lowMagic
    M.Calloc _ num size ->
      collect num ++ collect size
    M.Malloc _ size ->
      collect size
    M.MallocOpaque _ size ->
      collect size
    M.Realloc _ ptr size ->
      collect ptr ++ collect size
    M.Free _ ptr ->
      collect ptr
    M.InspectType {} ->
      []
    M.EqType {} ->
      []
    M.ShowType _ ->
      []
    M.TextCons rune text ->
      collect rune ++ collect text
    M.TextUncons _ text ->
      collect text
    M.MakeSwitch _ key fallback clauses ->
      concatMap collect [key, fallback, clauses]
    M.FreeVariables _ code ->
      collect code
    M.CompileError msg ->
      collect msg
    M.GetOriginFileName ->
      []
    M.GetOriginLine ->
      []
    M.GetOriginColumn ->
      []

collectLowMagic :: LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> [(Hint, Ident)]
collectLowMagic lowMagic =
  case lowMagic of
    LM.Cast _ _ value ->
      collect value
    LM.Store _ _ value pointer ->
      collect value ++ collect pointer
    LM.Load _ pointer ->
      collect pointer
    LM.Shift pointer offset ->
      collect pointer ++ collect offset
    LM.Alloca _ size ->
      collect size
    LM.External _ _ _ args varArgs -> do
      let argVars = concatMap collect args
      let varArgVars = concatMap (collect . fst) varArgs
      argVars ++ varArgVars
    LM.Global {} ->
      []
    LM.OpaqueValue e ->
      collect e
    LM.CallType func arg1 arg2 arg3 ->
      concatMap collect [func, arg1, arg2, arg3]
