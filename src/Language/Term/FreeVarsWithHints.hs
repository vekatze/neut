module Language.Term.FreeVarsWithHints (freeVarsWithHints, freeVarsWithHintsType) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

freeVarsWithHints :: TM.Term -> S.Set (Hint, Ident)
freeVarsWithHints term =
  case term of
    m :< TM.Var x ->
      S.singleton (m, x)
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.PiIntro k impArgs expArgs defaultArgs e -> do
      let impBinders = impArgs ++ expArgs
      let defaultVars = S.unions $ map freeVarsWithHints $ map snd defaultArgs
      S.union defaultVars (freeVarsWithHintsBinderType (impBinders ++ map fst defaultArgs ++ catMaybes [AttrL.fromAttr k]) (freeVarsWithHints e))
    _ :< TM.PiElim _ e impArgs expArgs -> do
      let xs = freeVarsWithHints e
      let ys1 = S.unions $ map freeVarsWithHintsType impArgs
      let ys2 = S.unions $ map freeVarsWithHints expArgs
      S.unions [xs, ys1, ys2]
    _ :< TM.DataIntro attr _ dataArgs consArgs -> do
      let xs1 = S.unions $ map freeVarsWithHintsType dataArgs
      let xs2 = S.unions $ map freeVarsWithHints consArgs
      let xs3 = freeVarsWithHintsAttrDataIntro attr
      S.unions [xs1, xs2, xs3]
    m :< TM.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVarsWithHints es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVarsWithHintsBinderType binder (freeVarsWithHintsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      freeVarsWithHintsBinderType xts (S.unions $ map freeVarsWithHints (e : es))
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVarsWithHintsBinderType xts (S.unions $ map freeVarsWithHints $ es ++ [e2])
    _ :< TM.CodeIntro e ->
      freeVarsWithHints e
    _ :< TM.CodeElim e ->
      freeVarsWithHints e
    _ :< TM.TauIntro ty ->
      freeVarsWithHintsType ty
    _ :< TM.TauElim (m, x) e1 e2 ->
      S.union (freeVarsWithHints e1) (S.delete (m, x) (freeVarsWithHints e2))
    _ :< TM.Let _ mxt e1 e2 -> do
      let set1 = freeVarsWithHints e1
      let set2 = freeVarsWithHintsBinderType [mxt] (freeVarsWithHints e2)
      S.union set1 set2
    _ :< TM.Prim prim ->
      case prim of
        PV.StaticText t _ ->
          freeVarsWithHintsType t
        PV.Int t _ _ ->
          freeVarsWithHintsType t
        PV.Float t _ _ ->
          freeVarsWithHintsType t
        _ ->
          S.empty
    _ :< TM.Magic der ->
      freeVarsWithHintsMagic der

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
    _ :< TM.Pi _ impArgs expArgs defaultArgs t -> do
      let impBinders = impArgs ++ expArgs
      let defaultVars = S.unions $ map freeVarsWithHints $ map snd defaultArgs
      S.union defaultVars (freeVarsWithHintsBinderType (impBinders ++ map fst defaultArgs) (freeVarsWithHintsType t))
    _ :< TM.Data attr _ es -> do
      let xs1 = S.unions $ map freeVarsWithHintsType es
      let xs2 = freeVarsWithHintsAttrData attr
      S.union xs1 xs2
    _ :< TM.Box t ->
      freeVarsWithHintsType t
    _ :< TM.BoxNoema t ->
      freeVarsWithHintsType t
    _ :< TM.Code t ->
      freeVarsWithHintsType t
    _ :< TM.PrimType {} ->
      S.empty
    _ :< TM.Void ->
      S.empty
    _ :< TM.Resource _ _ unitType discarder copier typeTag -> do
      let xs1 = freeVarsWithHintsType unitType
      let xs2 = freeVarsWithHints discarder
      let xs3 = freeVarsWithHints copier
      let xs4 = freeVarsWithHints typeTag
      S.unions [xs1, xs2, xs3, xs4]

freeVarsWithHintsBinderType :: [BinderF TM.Type] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
freeVarsWithHintsBinderType binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVarsWithHintsType t
      let hs2 = freeVarsWithHintsBinderType xts zs
      S.union hs1 $ S.filter (\(_, y) -> y /= x) hs2

freeVarsWithHintsDecisionTree :: DT.DecisionTree TM.Type TM.Term -> S.Set (Hint, Ident)
freeVarsWithHintsDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVarsWithHints (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVarsWithHintsType cursor) (freeVarsWithHintsCaseList caseList)

freeVarsWithHintsCaseList :: DT.CaseList TM.Type TM.Term -> S.Set (Hint, Ident)
freeVarsWithHintsCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsWithHintsDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsWithHintsCase clauseList
  S.union xs1 xs2

freeVarsWithHintsCase :: DT.Case TM.Type TM.Term -> S.Set (Hint, Ident)
freeVarsWithHintsCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      freeVarsWithHintsDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ freeVarsWithHintsBinderType consArgs (freeVarsWithHintsDecisionTree cont) : map freeVarsWithHintsType dataTerms ++ map freeVarsWithHintsType dataTypes

freeVarsWithHintsAttrData :: AttrD.Attr name (BinderF TM.Type) -> S.Set (Hint, Ident)
freeVarsWithHintsAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsWithHintsType t) binders) consNameList

freeVarsWithHintsAttrDataIntro :: AttrDI.Attr name (BinderF TM.Type) -> S.Set (Hint, Ident)
freeVarsWithHintsAttrDataIntro attr = do
  let consNameList = AttrDI.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsWithHintsType t) binders) consNameList

freeVarsWithHintsMagic :: M.Magic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
freeVarsWithHintsMagic magic =
  case magic of
    M.LowMagic lowMagic ->
      freeVarsWithHintsLowMagic lowMagic
    M.GetTypeTag _ typeTagExpr e ->
      S.union (freeVarsWithHintsType typeTagExpr) (freeVarsWithHintsType e)
    M.GetDataArgs _ listExpr typeExpr ->
      S.union (freeVarsWithHintsType listExpr) (freeVarsWithHintsType typeExpr)
    M.GetConsSize typeExpr ->
      freeVarsWithHintsType typeExpr
    M.GetWrapperContentType typeExpr ->
      freeVarsWithHintsType typeExpr
    M.GetVectorContentType _ typeExpr ->
      freeVarsWithHintsType typeExpr
    M.GetNoemaContentType typeExpr ->
      freeVarsWithHintsType typeExpr
    M.GetBoxContentType typeExpr ->
      freeVarsWithHintsType typeExpr
    M.GetConstructorArgTypes _ listExpr typeExpr index ->
      S.unions [freeVarsWithHintsType listExpr, freeVarsWithHintsType typeExpr, freeVarsWithHints index]
    M.GetConsName textType typeExpr index ->
      S.unions [freeVarsWithHintsType textType, freeVarsWithHintsType typeExpr, freeVarsWithHints index]
    M.GetConsConstFlag boolType typeExpr index ->
      S.unions [freeVarsWithHintsType boolType, freeVarsWithHintsType typeExpr, freeVarsWithHints index]
    M.ShowType textTypeExpr typeExpr ->
      S.union (freeVarsWithHintsType textTypeExpr) (freeVarsWithHintsType typeExpr)
    M.TextCons textTypeExpr rune text ->
      S.unions [freeVarsWithHintsType textTypeExpr, freeVarsWithHints rune, freeVarsWithHints text]
    M.TextUncons _ text ->
      freeVarsWithHints text
    M.CompileError typeExpr msg ->
      S.union (freeVarsWithHintsType typeExpr) (freeVarsWithHints msg)

freeVarsWithHintsLowMagic :: LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> S.Set (Hint, Ident)
freeVarsWithHintsLowMagic lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [freeVarsWithHintsType from, freeVarsWithHintsType to, freeVarsWithHints value]
    LM.Store _ _ value pointer ->
      S.unions [freeVarsWithHints value, freeVarsWithHints pointer]
    LM.Load _ pointer ->
      freeVarsWithHints pointer
    LM.Alloca _ size ->
      freeVarsWithHints size
    LM.External _ _ _ args varArgs -> do
      let argVars = S.unions $ map freeVarsWithHints args
      let varArgVars = S.unions $ map (\(arg, _) -> freeVarsWithHints arg) varArgs
      S.unions [argVars, varArgVars]
    LM.Global {} ->
      S.empty
    LM.OpaqueValue e ->
      freeVarsWithHints e
    LM.CallType func arg1 arg2 ->
      S.unions [freeVarsWithHints func, freeVarsWithHints arg1, freeVarsWithHints arg2]
