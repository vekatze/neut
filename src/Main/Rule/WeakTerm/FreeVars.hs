module Main.Rule.WeakTerm.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Main.Rule.Annotation qualified as AN
import Main.Rule.Attr.Lam qualified as AttrL
import Main.Rule.Binder
import Main.Rule.DecisionTree qualified as DT
import Main.Rule.Ident
import Main.Rule.WeakTerm qualified as WT

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi impArgs expArgs t ->
      freeVars' (impArgs ++ expArgs) (freeVars t)
    _ :< WT.PiIntro k impArgs expArgs e ->
      freeVars' (impArgs ++ expArgs ++ catMaybes [AttrL.fromAttr k]) (freeVars e)
    _ :< WT.PiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< WT.PiElimExact e -> do
      freeVars e
    _ :< WT.Data _ _ es ->
      S.unions $ map freeVars es
    _ :< WT.DataIntro _ _ dataArgs consArgs -> do
      S.unions $ map freeVars $ dataArgs ++ consArgs
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
    _ :< WT.BoxIntroQuote e ->
      freeVars e
    _ :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let (xts, es) = unzip $ castSeq ++ [(mxt, e1)] ++ uncastSeq
      freeVars' xts (S.unions $ map freeVars $ es ++ [e2])
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
    _ :< WT.Resource _ _ unitType discarder copier -> do
      let xs1 = freeVars unitType
      let xs2 = freeVars discarder
      let xs3 = freeVars copier
      S.unions [xs1, xs2, xs3]
    _ :< WT.Use e xts cont -> do
      let fvs1 = freeVars e
      let fvs2 = freeVars' xts (freeVars cont)
      S.union fvs1 fvs2
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
