module Entity.WeakTerm.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Entity.Annotation qualified as AN
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Ident
import Entity.LamKind
import Entity.WeakTerm qualified as WT

freeVars :: WT.WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.Var x ->
      S.singleton x
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi xts t ->
      freeVars' xts (freeVars t)
    _ :< WT.PiIntro k xts e ->
      freeVars' (catMaybes [fromLamKind k] ++ xts) (freeVars e)
    _ :< WT.PiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< WT.Data _ _ es ->
      S.unions $ map freeVars es
    _ :< WT.DataIntro _ _ _ _ dataArgs consArgs -> do
      S.unions $ map freeVars $ dataArgs ++ consArgs
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVars es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVars' binder (freeVarsDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.Noema t ->
      freeVars t
    _ :< WT.Embody t e ->
      S.union (freeVars t) (freeVars e)
    _ :< WT.Cell t ->
      freeVars t
    _ :< WT.CellIntro e ->
      freeVars e
    _ :< WT.CellElim e ->
      freeVars e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' [mxt] (freeVars e2)
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap freeVars prim
    _ :< WT.Hole _ es ->
      S.unions $ map freeVars es
    _ :< WT.ResourceType {} ->
      S.empty
    _ :< WT.Magic der ->
      foldMap freeVars der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVars e
      case annot of
        AN.Type t -> do
          let xs2 = freeVars t
          S.union xs1 xs2
    _ :< WT.Flow _ t -> do
      freeVars t
    _ :< WT.FlowIntro _ _ (e, t) -> do
      let xs1 = freeVars e
      let xs2 = freeVars t
      S.unions [xs1, xs2]
    _ :< WT.FlowElim _ _ (e, t) -> do
      let xs1 = freeVars e
      let xs2 = freeVars t
      S.unions [xs1, xs2]

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
    DT.Leaf _ e ->
      freeVars e
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
freeVarsCase (DT.Cons _ _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  S.unions $ freeVars' consArgs (freeVarsDecisionTree tree) : map freeVars dataTerms ++ map freeVars dataTypes
