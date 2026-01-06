module Language.Term.FreeVars (freeVars, freeVarsType) where

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

freeVars :: TM.Term -> S.Set Ident
freeVars term =
  case term of
    _ :< TM.Var x ->
      S.singleton x
    _ :< TM.VarGlobal {} ->
      S.empty
    _ :< TM.PiIntro k impArgs defaultArgs expArgs e -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVarsBinderType (impBinders ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars e))
    _ :< TM.PiElim _ e impArgs expArgs -> do
      let xs = freeVars e
      let ys1 = S.unions $ map freeVarsType impArgs
      let ys2 = S.unions $ map freeVars expArgs
      S.unions [xs, ys1, ys2]
    _ :< TM.DataIntro attr _ dataArgs consArgs -> do
      let xs1 = S.unions $ map freeVarsType dataArgs
      let xs2 = S.unions $ map freeVars consArgs
      let xs3 = freeVarsAttrDataIntro attr
      S.unions [xs1, xs2, xs3]
    m :< TM.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVarsBinderType binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      freeVarsBinderType xts (S.unions $ map freeVars (e : es))
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVarsBinderType xts (S.unions $ map freeVars $ es ++ [e2])
    _ :< TM.CodeIntro e ->
      freeVars e
    _ :< TM.CodeElim e ->
      freeVars e
    _ :< TM.TauIntro ty ->
      freeVarsType ty
    _ :< TM.TauElim (_, x) e1 e2 ->
      S.union (freeVars e1) (S.delete x (freeVars e2))
    _ :< TM.Let _ mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVarsBinderType [mxt] (freeVars e2)
      S.union set1 set2
    _ :< TM.Prim prim ->
      case prim of
        PV.StaticText t _ ->
          freeVarsType t
        PV.Int t _ _ ->
          freeVarsType t
        PV.Float t _ _ ->
          freeVarsType t
        _ ->
          S.empty
    _ :< TM.Magic der ->
      freeVarsMagic der

freeVarsType :: TM.Type -> S.Set Ident
freeVarsType ty =
  case ty of
    _ :< TM.Tau ->
      S.empty
    _ :< TM.TVar x ->
      S.singleton x
    _ :< TM.TVarGlobal {} ->
      S.empty
    _ :< TM.TyApp t args ->
      S.unions $ freeVarsType t : map freeVarsType args
    _ :< TM.Pi _ impArgs defaultArgs expArgs t -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVarsBinderType (impBinders ++ expArgs) (freeVarsType t))
    _ :< TM.Data attr _ es -> do
      let xs1 = S.unions $ map freeVarsType es
      let xs2 = freeVarsAttrData attr
      S.union xs1 xs2
    _ :< TM.Box t ->
      freeVarsType t
    _ :< TM.BoxNoema t ->
      freeVarsType t
    _ :< TM.Code t ->
      freeVarsType t
    _ :< TM.PrimType {} ->
      S.empty
    _ :< TM.Void ->
      S.empty
    _ :< TM.Resource _ _ unitType discarder copier typeTag -> do
      let xs1 = freeVarsType unitType
      let xs2 = freeVars discarder
      let xs3 = freeVars copier
      let xs4 = freeVars typeTag
      S.unions [xs1, xs2, xs3, xs4]

freeVarsBinderType :: [BinderF TM.Type] -> S.Set Ident -> S.Set Ident
freeVarsBinderType binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVarsType t
      let hs2 = freeVarsBinderType xts zs
      S.union hs1 $ S.filter (/= x) hs2

freeVarsDecisionTree :: DT.DecisionTree TM.Type TM.Term -> S.Set Ident
freeVarsDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVars (TM.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVarsType cursor) (freeVarsCaseList caseList)

freeVarsCaseList :: DT.CaseList TM.Type TM.Term -> S.Set Ident
freeVarsCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsCase clauseList
  S.union xs1 xs2

freeVarsCase :: DT.Case TM.Type TM.Term -> S.Set Ident
freeVarsCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      freeVarsDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ freeVarsBinderType consArgs (freeVarsDecisionTree cont) : map freeVarsType dataTerms ++ map freeVarsType dataTypes

freeVarsAttrData :: AttrD.Attr name (BinderF TM.Type) -> S.Set Ident
freeVarsAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsType t) binders) consNameList

freeVarsAttrDataIntro :: AttrDI.Attr name (BinderF TM.Type) -> S.Set Ident
freeVarsAttrDataIntro attr = do
  let consNameList = AttrDI.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsType t) binders) consNameList

freeVarsMagic :: M.Magic BLT.BaseLowType TM.Type TM.Term -> S.Set Ident
freeVarsMagic magic =
  case magic of
    M.LowMagic lowMagic ->
      freeVarsLowMagic lowMagic
    M.GetTypeTag _ typeTagExpr e ->
      S.union (freeVarsType typeTagExpr) (freeVarsType e)
    M.GetDataArgs _ listExpr typeExpr ->
      S.union (freeVarsType listExpr) (freeVarsType typeExpr)
    M.GetConsSize typeExpr ->
      freeVarsType typeExpr
    M.GetConstructorArgTypes _ listExpr typeExpr index ->
      S.unions [freeVarsType listExpr, freeVarsType typeExpr, freeVars index]
    M.CompileError _ ->
      S.empty

freeVarsLowMagic :: LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> S.Set Ident
freeVarsLowMagic lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [freeVarsType from, freeVarsType to, freeVars value]
    LM.Store _ _ value pointer ->
      S.unions [freeVars value, freeVars pointer]
    LM.Load _ pointer ->
      freeVars pointer
    LM.Alloca _ size ->
      freeVars size
    LM.External _ _ _ args varArgs -> do
      let argVars = S.unions $ map freeVars args
      let varArgVars = S.unions $ map (\(arg, _) -> freeVars arg) varArgs
      S.unions [argVars, varArgVars]
    LM.Global {} ->
      S.empty
    LM.OpaqueValue e ->
      freeVars e
    LM.CallType func arg1 arg2 ->
      S.unions [freeVars func, freeVars arg1, freeVars arg2]
