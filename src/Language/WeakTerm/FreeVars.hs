module Language.WeakTerm.FreeVars (freeVars, freeVarsType) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.WeakTerm qualified as WT

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.PiIntro k impArgs defaultArgs expArgs e -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVarsBindersType (impBinders ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars e))
    _ :< WT.PiElim _ e impArgs defaultArgs expArgs -> do
      let xs = freeVars e
      let ys1 = S.unions $ map freeVarsType (ImpArgs.extract impArgs)
      let ys2 = S.unions $ map freeVars (DefaultArgs.extract defaultArgs)
      let ys3 = S.unions $ map freeVars expArgs
      S.unions [xs, ys1, ys2, ys3]
    _ :< WT.PiElimExact e -> do
      freeVars e
    _ :< WT.DataIntro attr _ dataArgs consArgs -> do
      let xs1 = S.unions $ map freeVars consArgs
      let xs2 = S.unions $ map freeVarsType dataArgs
      let xs3 = freeVarsAttrDataIntro attr
      S.unions [xs1, xs2, xs3]
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVarsBindersType binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.BoxIntro letSeq e -> do
      let (mxts, es) = unzip letSeq
      freeVarsBindersType mxts (S.unions $ map freeVars (e : es))
    _ :< WT.BoxIntroLift e ->
      freeVars e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVarsBindersType xts (S.unions $ map freeVars $ es ++ [e2])
    _ :< WT.CodeIntro e ->
      freeVars e
    _ :< WT.CodeElim e ->
      freeVars e
    _ :< WT.Actual e ->
      freeVars e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVarsBindersType [mxt] (freeVars e2)
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap freeVarsType prim
    _ :< WT.Magic der ->
      freeVarsMagic der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVars e
      case annot of
        AN.Type t -> do
          let xs2 = freeVarsType t
          S.union xs1 xs2

freeVarsType :: WT.WeakType -> S.Set Ident
freeVarsType ty =
  case ty of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.TVar x ->
      S.singleton x
    _ :< WT.TVarGlobal {} ->
      S.empty
    _ :< WT.TyApp t args -> do
      S.unions $ freeVarsType t : map freeVarsType args
    _ :< WT.Pi _ impArgs defaultArgs expArgs t -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVarsBindersType (impBinders ++ expArgs) (freeVarsType t))
    _ :< WT.Data attr _ es -> do
      let xs1 = S.unions $ map freeVarsType es
      let xs2 = freeVarsAttrData attr
      S.union xs1 xs2
    _ :< WT.Box t ->
      freeVarsType t
    _ :< WT.BoxNoema t ->
      freeVarsType t
    _ :< WT.Code t ->
      freeVarsType t
    _ :< WT.PrimType {} ->
      S.empty
    _ :< WT.Void ->
      S.empty
    _ :< WT.Resource _ _ unitType discarder copier typeTag -> do
      S.unions [freeVarsType unitType, freeVars discarder, freeVars copier, freeVars typeTag]
    _ :< WT.TypeHole _ es ->
      S.unions $ map freeVarsType es

freeVarsBindersType :: [BinderF WT.WeakType] -> S.Set Ident -> S.Set Ident
freeVarsBindersType binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVarsType t
      let hs2 = freeVarsBindersType xts zs
      S.union hs1 $ S.filter (/= x) hs2

freeVarsDecisionTree :: DT.DecisionTree WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVars (WT.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVarsType cursor) (freeVarsCaseList caseList)

freeVarsCaseList :: DT.CaseList WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsCase clauseList
  S.union xs1 xs2

freeVarsCase :: DT.Case WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      freeVarsDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTypes1, dataTypes2) = unzip dataArgs
      S.unions $ freeVarsBindersType consArgs (freeVarsDecisionTree cont) : map freeVarsType dataTypes1 ++ map freeVarsType dataTypes2

freeVarsMagic :: M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsMagic (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      freeVarsLowMagic lowMagic
    M.GetTypeTag _ typeTagExpr e ->
      S.union (freeVarsType typeTagExpr) (freeVarsType e)
    M.GetConsSize typeExpr ->
      freeVarsType typeExpr
    M.GetConstructorArgTypes _ listExpr typeExpr index ->
      S.unions [freeVarsType listExpr, freeVarsType typeExpr, freeVars index]
    M.CompileError _ ->
      S.empty

freeVarsLowMagic :: LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> S.Set Ident
freeVarsLowMagic lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      S.unions [freeVarsType from, freeVarsType to, freeVars value]
    LM.Store t unit value pointer ->
      S.unions [freeVarsType t, freeVarsType unit, freeVars value, freeVars pointer]
    LM.Load t pointer ->
      S.union (freeVarsType t) (freeVars pointer)
    LM.Alloca t size ->
      S.union (freeVarsType t) (freeVars size)
    LM.External domList cod _ args varArgs -> do
      let domVars = S.unions $ map freeVarsType domList
      let codVars = case cod of
            FCT.Cod t -> freeVarsType t
            FCT.Void -> S.empty
      let argsVars = S.unions $ map freeVars args
      let varArgsVars = S.unions $ map (\(a, t) -> S.union (freeVars a) (freeVarsType t)) varArgs
      S.unions [domVars, codVars, argsVars, varArgsVars]
    LM.Global _ t ->
      freeVarsType t
    LM.OpaqueValue e ->
      freeVars e
    LM.CallType func arg1 arg2 ->
      S.unions [freeVarsType func, freeVars arg1, freeVars arg2]
    LM.TermType ty ->
      freeVarsType ty

freeVarsAttrData :: AttrD.Attr name (BinderF WT.WeakType) -> S.Set Ident
freeVarsAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsType t) binders) consNameList

freeVarsAttrDataIntro :: AttrDI.Attr name (BinderF WT.WeakType) -> S.Set Ident
freeVarsAttrDataIntro attr = do
  let consNameList = AttrDI.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVarsType t) binders) consNameList
