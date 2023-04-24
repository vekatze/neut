module Entity.WeakTerm.FreeVarsWithHint (freeVarsWithHint) where

import Control.Comonad.Cofree
import Data.Maybe
import Data.Set qualified as S
import Entity.Annotation qualified as AN
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Hint
import Entity.Ident
import Entity.LamKind
import Entity.WeakTerm qualified as WT

freeVarsWithHint :: WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsWithHint term =
  case term of
    _ :< WT.Tau ->
      S.empty
    m :< WT.Var x ->
      S.singleton (m, x)
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi xts t ->
      freeVarsWithHint' xts (freeVarsWithHint t)
    _ :< WT.PiIntro k xts e ->
      freeVarsWithHint' (catMaybes [fromLamKind k] ++ xts) (freeVarsWithHint e)
    _ :< WT.PiElim e es -> do
      let xs = freeVarsWithHint e
      let ys = S.unions $ map freeVarsWithHint es
      S.union xs ys
    _ :< WT.Data _ _ es ->
      S.unions $ map freeVarsWithHint es
    _ :< WT.DataIntro _ _ _ _ dataArgs consArgs -> do
      S.unions $ map freeVarsWithHint $ dataArgs ++ consArgs
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map freeVarsWithHint es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = freeVarsWithHint' binder (freeVarsWithHintDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.Noema t ->
      freeVarsWithHint t
    _ :< WT.Embody t e ->
      S.union (freeVarsWithHint t) (freeVarsWithHint e)
    _ :< WT.Cell t ->
      freeVarsWithHint t
    _ :< WT.CellIntro e ->
      freeVarsWithHint e
    _ :< WT.CellElim e ->
      freeVarsWithHint e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = freeVarsWithHint e1
      let set2 = freeVarsWithHint' [mxt] (freeVarsWithHint e2)
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap freeVarsWithHint prim
    _ :< WT.Hole _ es ->
      S.unions $ map freeVarsWithHint es
    _ :< WT.ResourceType {} ->
      S.empty
    _ :< WT.Magic der ->
      foldMap freeVarsWithHint der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = freeVarsWithHint e
      case annot of
        AN.Type t -> do
          let xs2 = freeVarsWithHint t
          S.union xs1 xs2
    _ :< WT.Promise _ t -> do
      freeVarsWithHint t
    _ :< WT.PromiseIntro _ _ (e, t) -> do
      let xs1 = freeVarsWithHint e
      let xs2 = freeVarsWithHint t
      S.unions [xs1, xs2]
    _ :< WT.PromiseElim _ _ (e, t) -> do
      let xs1 = freeVarsWithHint e
      let xs2 = freeVarsWithHint t
      S.unions [xs1, xs2]

freeVarsWithHint' :: [BinderF WT.WeakTerm] -> S.Set (Hint, Ident) -> S.Set (Hint, Ident)
freeVarsWithHint' binder zs =
  case binder of
    [] ->
      zs
    ((m, x, t) : xts) -> do
      let hs1 = freeVarsWithHint t
      let hs2 = freeVarsWithHint' xts zs
      S.union hs1 $ S.filter (/= (m, x)) hs2

freeVarsWithHintDecisionTree :: DT.DecisionTree WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsWithHintDecisionTree tree =
  case tree of
    DT.Leaf _ e ->
      freeVarsWithHint e
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (freeVarsWithHint cursor) (freeVarsWithHintCaseList caseList)

freeVarsWithHintCaseList :: DT.CaseList WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsWithHintCaseList (fallbackClause, clauseList) = do
  let xs1 = freeVarsWithHintDecisionTree fallbackClause
  let xs2 = S.unions $ map freeVarsWithHintCase clauseList
  S.union xs1 xs2

freeVarsWithHintCase :: DT.Case WT.WeakTerm -> S.Set (Hint, Ident)
freeVarsWithHintCase (DT.Cons _ _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  S.unions $ freeVarsWithHint' consArgs (freeVarsWithHintDecisionTree tree) : map freeVarsWithHint dataTerms ++ map freeVarsWithHint dataTypes
