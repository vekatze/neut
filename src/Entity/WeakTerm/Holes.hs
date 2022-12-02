module Entity.WeakTerm.Holes (holes) where

import Control.Comonad.Cofree
import qualified Data.Set as S
import Entity.Binder
import qualified Entity.DecisionTree as DT
import Entity.HoleID
import qualified Entity.WeakTerm as WT

holes :: WT.WeakTerm -> S.Set HoleID
holes term =
  case term of
    _ :< WT.Tau ->
      S.empty
    _ :< WT.Var {} ->
      S.empty
    _ :< WT.VarGlobal {} ->
      S.empty
    _ :< WT.Pi xts t ->
      holes' xts (holes t)
    _ :< WT.PiIntro _ xts e ->
      holes' xts (holes e)
    _ :< WT.PiElim e es ->
      S.unions $ map holes $ e : es
    _ :< WT.Data _ es ->
      S.unions $ map holes es
    _ :< WT.DataIntro _ _ _ dataArgs consArgs -> do
      S.unions $ map holes $ dataArgs ++ consArgs
    m :< WT.DataElim oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map holes es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = holes' binder (holesDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.Sigma xts ->
      holes' xts S.empty
    _ :< WT.SigmaIntro es ->
      S.unions $ map holes es
    _ :< WT.SigmaElim xts e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' xts (holes e2)
      S.union set1 set2
    _ :< WT.Let mxt e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' [mxt] (holes e2)
      S.union set1 set2
    _ :< WT.Aster h es ->
      S.insert h $ S.unions $ map holes es
    _ :< WT.Prim prim ->
      foldMap holes prim
    _ :< WT.Enum {} ->
      S.empty
    _ :< WT.EnumIntro {} ->
      S.empty
    _ :< WT.EnumElim (e, t) les -> do
      let set1 = holes e
      let set2 = holes t
      let set3 = S.unions $ map (\(_, body) -> holes body) les
      S.unions [set1, set2, set3]
    _ :< WT.Question e t -> do
      let set1 = holes e
      let set2 = holes t
      S.union set1 set2
    _ :< WT.Magic der ->
      foldMap holes der

holes' :: [BinderF WT.WeakTerm] -> S.Set HoleID -> S.Set HoleID
holes' binder zs =
  case binder of
    [] ->
      zs
    ((_, _, t) : xts) -> do
      let set1 = holes t
      let set2 = holes' xts zs
      S.union set1 set2

holesDecisionTree :: DT.DecisionTree WT.WeakTerm -> S.Set HoleID
holesDecisionTree tree =
  case tree of
    DT.Leaf _ e ->
      holes e
    DT.Unreachable ->
      S.empty
    DT.Switch (_, cursor) caseList ->
      S.union (holes cursor) (holesCaseList caseList)

holesCaseList :: DT.CaseList WT.WeakTerm -> S.Set HoleID
holesCaseList (fallbackClause, clauseList) = do
  let xs1 = holesDecisionTree fallbackClause
  let xs2 = S.unions $ map holesCase clauseList
  S.union xs1 xs2

holesCase :: DT.Case WT.WeakTerm -> S.Set HoleID
holesCase (DT.Cons _ _ dataArgs consArgs tree) =
  S.unions $ holes' consArgs (holesDecisionTree tree) : map holes dataArgs
