module Language.WeakTerm.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.WeakTerm.WeakTerm qualified as WT

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi _ impArgs defaultArgs expArgs t -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVars' (impBinders ++ expArgs) (freeVars t))
    _ :< WT.PiIntro k impArgs defaultArgs expArgs e -> do
      let impBinders = impArgs ++ map fst defaultArgs
      let defaultVars = S.unions $ map freeVars $ map snd defaultArgs
      S.union defaultVars (freeVars' (impBinders ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars e))
    _ :< WT.PiElim _ e impArgs expArgs -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars (ImpArgs.extract impArgs ++ expArgs)
      S.union xs ys
    _ :< WT.PiElimExact e -> do
      freeVars e
    _ :< WT.Data attr _ es -> do
      let xs1 = S.unions $ map freeVars es
      let xs2 = freeVarsAttrData attr
      S.union xs1 xs2
    _ :< WT.DataIntro attr _ dataArgs consArgs -> do
      let xs1 = S.unions $ map freeVars $ dataArgs ++ consArgs
      let xs2 = freeVarsAttrDataIntro attr
      S.union xs1 xs2
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVars' binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.Box t ->
      freeVars t
    _ :< WT.BoxNoema t ->
      freeVars t
    _ :< WT.BoxIntro letSeq e -> do
      let (mxts, es) = unzip letSeq
      freeVars' mxts (S.unions $ map freeVars (e : es))
    _ :< WT.BoxIntroLift e ->
      freeVars e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVars' xts (S.unions $ map freeVars $ es ++ [e2])
    _ :< WT.Code t ->
      freeVars t
    _ :< WT.CodeIntro e ->
      freeVars e
    _ :< WT.CodeElim e ->
      freeVars e
    _ :< WT.Actual e ->
      freeVars e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' [mxt] (freeVars e2)
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap freeVars prim
    _ :< WT.Hole _ es ->
      S.unions $ map freeVars es
    _ :< WT.Magic der ->
      foldMap freeVars der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVars e
      case annot of
        AN.Type t -> do
          let xs2 = freeVars t
          S.union xs1 xs2
    _ :< WT.Resource _ _ unitType discarder copier typeTag -> do
      let xs1 = freeVars unitType
      let xs2 = freeVars discarder
      let xs3 = freeVars copier
      let xs4 = freeVars typeTag
      S.unions [xs1, xs2, xs3, xs4]
    _ :< WT.Void ->
      S.empty

freeVars' :: [BinderF WT.WeakTerm] -> S.Set Ident -> S.Set Ident
freeVars' binder zs =
  case binder of
    [] ->
      zs
    ((_, x, t) : xts) -> do
      let hs1 = freeVars t
      let hs2 = freeVars' xts zs
      S.union hs1 $ S.filter (/= x) hs2

freeVarsDecisionTree :: DT.DecisionTree WT.WeakTerm -> S.Set Ident
freeVarsDecisionTree tree =
  case tree of
    DT.Leaf _ letSeq e ->
      freeVars (WT.fromLetSeq letSeq e)
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVars cursor) (freeVarsCaseList caseList)

freeVarsCaseList :: DT.CaseList WT.WeakTerm -> S.Set Ident
freeVarsCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsCase clauseList
  S.union xs1 xs2

freeVarsCase :: DT.Case WT.WeakTerm -> S.Set Ident
freeVarsCase decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont -> do
      freeVarsDecisionTree cont
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      S.unions $ freeVars' consArgs (freeVarsDecisionTree cont) : map freeVars dataTerms ++ map freeVars dataTypes

freeVarsAttrData :: AttrD.Attr name (BinderF WT.WeakTerm) -> S.Set Ident
freeVarsAttrData attr = do
  let consNameList = AttrD.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVars t) binders) consNameList

freeVarsAttrDataIntro :: AttrDI.Attr name (BinderF WT.WeakTerm) -> S.Set Ident
freeVarsAttrDataIntro attr = do
  let consNameList = AttrDI.consNameList attr
  S.unions $ map (\(_, binders, _) -> S.unions $ map (\(_, _, t) -> freeVars t) binders) consNameList
