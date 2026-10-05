module Language.Term.FreeVarsWithHints
  ( freeVarsWithHints,
    freeTermVarsWithHints,
  )
where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CallConv qualified as CC
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.VarKind qualified as VK
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

freeVarsWithHints :: TM.Term -> S.Set (Hint, Ident)
freeVarsWithHints =
  collect freeVarsWithHintsType

freeTermVarsWithHints :: TM.Term -> S.Set (Hint, Ident)
freeTermVarsWithHints =
  collect (const S.empty)

collect :: (TM.Type -> S.Set (Hint, Ident)) -> TM.Term -> S.Set (Hint, Ident)
collect visitType term =
  case term of
    m :< TM.Var x ->
      S.singleton (m, x)
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.PiIntro k impArgs expArgs defaultArgs e -> do
      let impBinders = impArgs ++ expArgs
      let defaultVars = S.unions $ map (collect visitType) $ map snd defaultArgs
      S.union defaultVars (collectBinder visitType (impBinders ++ map fst defaultArgs ++ catMaybes [AttrL.fromAttr k]) (collect visitType e))
    _ :< TM.PiElim _ conv e impArgs expArgs defaultArgs -> do
      let xs = collect visitType e
      let ys0 = S.unions $ map visitType (CC.types conv)
      let ys1 = S.unions $ map visitType impArgs
      let ys2 = S.unions $ map (collect visitType) expArgs
      let ys3 = S.unions $ map (collect visitType) (catMaybes defaultArgs)
      S.unions [xs, ys0, ys1, ys2, ys3]
    _ :< TM.DataIntro _ _ dataArgs consImpArgs consArgs -> do
      let xs1 = S.unions $ map visitType (dataArgs ++ consImpArgs)
      let xs2 = S.unions $ map (collect visitType) consArgs
      S.union xs1 xs2
    m :< TM.DataElim _ _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map (collect visitType) es
      let binder = zipWith (\o t -> (m, VK.normal, o, t)) os ts
      let xs2 = collectBinder visitType binder (collectDecisionTree visitType decisionTree)
      S.union xs1 xs2
    _ :< TM.BoxIntro _ letSeq e -> do
      let (xts, es) = unzip letSeq
      collectBinder visitType xts (S.unions $ map (collect visitType) (e : es))
    _ :< TM.BoxIntroLift t e ->
      S.union (visitType t) (collect visitType e)
    _ :< TM.EmbedIntro e ->
      collect visitType e
    _ :< TM.BoxElim _ castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      collectBinder visitType xts (S.unions $ map (collect visitType) $ es ++ [e2])
    _ :< TM.CodeIntro _ e ->
      collect visitType e
    _ :< TM.CodeElim _ e ->
      collect visitType e
    _ :< TM.TauIntro ty ->
      visitType ty
    _ :< TM.TauElim _ (m, _, x) e1 e2 ->
      S.union (collect visitType e1) (S.delete (m, x) (collect visitType e2))
    _ :< TM.Let mxt e1 e2 -> do
      let set1 = collect visitType e1
      let set2 = collectBinder visitType [mxt] (collect visitType e2)
      S.union set1 set2
    _ :< TM.Invoke _ body ->
      collect visitType body
    _ :< TM.Prim prim ->
      case prim of
        PV.NoeticString t _ ->
          visitType t
        PV.NoeticBinary t _ ->
          visitType t
        PV.Text _ ->
          mempty
        PV.Blob _ ->
          mempty
        PV.Int t _ _ ->
          visitType t
        PV.Float t _ _ ->
          visitType t
        _ ->
          S.empty
    _ :< TM.Magic _ der ->
      collectMagic visitType der

freeVarsWithHintsType :: TM.Type -> S.Set (Hint, Ident)
freeVarsWithHintsType ty =
  case ty of
    _ :< TM.Tau ->
      S.empty
    m :< TM.TVar x ->
      S.singleton (m, x)
    _ :< TM.TVarGlobal {} ->
      S.empty
    _ :< TM.TyApp t args ->
      S.unions $ freeVarsWithHintsType t : map freeVarsWithHintsType args
    _ :< TM.Pi _ impArgs expArgs defaultArgs t ->
      collectBinder freeVarsWithHintsType (impArgs ++ expArgs ++ defaultArgs) (freeVarsWithHintsType t)
    _ :< TM.Data _ _ es ->
      S.unions $ map freeVarsWithHintsType es
    _ :< TM.Box t ->
      freeVarsWithHintsType t
    _ :< TM.BoxNoema t ->
      freeVarsWithHintsType t
    _ :< TM.Embed t ->
      freeVarsWithHintsType t
    _ :< TM.Code t ->
      freeVarsWithHintsType t
    _ :< TM.PrimType {} ->
      S.empty
    _ :< TM.Void ->
      S.empty
    _ :< TM.Resource _ _ -> do
      S.empty

collectBinder :: (TM.Type -> S.Set (Hint, Ident)) -> [BinderF TM.Type] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
collectBinder visitType binder zs =
  case binder of
    [] ->
      zs
    ((_, _, x, t) : xts) -> do
      let hs1 = visitType t
      let hs2 = collectBinder visitType xts zs
      S.union hs1 $ S.filter (\(_, y) -> y /= x) hs2

collectDecisionTree :: (TM.Type -> S.Set (Hint, Ident)) -> DT.DecisionTree TM.Type TM.Term -> S.Set (Hint, Ident)
collectDecisionTree visitType tree =
  case tree of
    DT.Leaf _ letSeq e ->
      collect visitType (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (visitType cursor) (collectCaseList visitType caseList)

collectCaseList :: (TM.Type -> S.Set (Hint, Ident)) -> DT.CaseList TM.Type TM.Term -> S.Set (Hint, Ident)
collectCaseList visitType (fallbackClause, clauseList) = do
  let xs1 = collectDecisionTree visitType fallbackClause
  let xs2 = S.unions $ map (collectCase visitType) clauseList
  S.union xs1 xs2

collectCase :: (TM.Type -> S.Set (Hint, Ident)) -> DT.Case TM.Type TM.Term -> S.Set (Hint, Ident)
collectCase visitType decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      collectDecisionTree visitType cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ collectBinder visitType (consImpArgs ++ consArgs) (collectDecisionTree visitType cont) : map visitType dataTerms ++ map visitType dataTypes

collectMagic :: (TM.Type -> S.Set (Hint, Ident)) -> M.Magic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
collectMagic visitType magic =
  case magic of
    M.LowMagic lowMagic ->
      collectLowMagic visitType lowMagic
    M.Calloc sizeType num size ->
      S.unions [visitType sizeType, collect visitType num, collect visitType size]
    M.Malloc sizeType size ->
      S.union (visitType sizeType) (collect visitType size)
    M.Realloc sizeType ptr size ->
      S.unions [visitType sizeType, collect visitType ptr, collect visitType size]
    M.Free unitType ptr ->
      S.union (visitType unitType) (collect visitType ptr)
    M.InspectType _ typeValueExpr e ->
      S.union (visitType typeValueExpr) (visitType e)
    M.EqType _ typeExpr1 typeExpr2 ->
      S.union (visitType typeExpr1) (visitType typeExpr2)
    M.ShowType typeExpr ->
      visitType typeExpr
    M.TextCons rune text ->
      S.union (collect visitType rune) (collect visitType text)
    M.TextUncons _ text ->
      collect visitType text
    M.MakeSwitch _ key fallback clauses ->
      S.unions [collect visitType key, collect visitType fallback, collect visitType clauses]
    M.FreeVariables _ code ->
      collect visitType code
    M.CompileError msg ->
      collect visitType msg
    M.GetOriginFileName ->
      S.empty
    M.GetOriginLine ->
      S.empty
    M.GetOriginColumn ->
      S.empty

collectLowMagic :: (TM.Type -> S.Set (Hint, Ident)) -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
collectLowMagic visitType lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [visitType from, visitType to, collect visitType value]
    LM.Store _ _ value pointer ->
      S.unions [collect visitType value, collect visitType pointer]
    LM.Load _ pointer ->
      collect visitType pointer
    LM.Alloca _ size ->
      collect visitType size
    LM.External _ _ _ args varArgs -> do
      let argVars = S.unions $ map (collect visitType) args
      let varArgVars = S.unions $ map (\(arg, _) -> collect visitType arg) varArgs
      S.unions [argVars, varArgVars]
    LM.Global {} ->
      S.empty
    LM.OpaqueValue e ->
      collect visitType e
    LM.CallType func arg1 arg2 arg3 ->
      S.unions [collect visitType func, collect visitType arg1, collect visitType arg2, collect visitType arg3]
