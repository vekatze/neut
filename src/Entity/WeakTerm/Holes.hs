module Entity.WeakTerm.Holes (holes) where

import Control.Comonad.Cofree
import Data.Set qualified as S
import Entity.Annotation qualified as AN
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.HoleID
import Entity.WeakTerm qualified as WT

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
    m :< WT.DataElim _ oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      let xs1 = S.unions $ map holes es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      let xs2 = holes' binder (holesDecisionTree decisionTree)
      S.union xs1 xs2
    _ :< WT.Noema t ->
      holes t
    _ :< WT.Embody t e ->
      S.union (holes t) (holes e)
    _ :< WT.Cell t ->
      holes t
    _ :< WT.CellIntro e ->
      holes e
    _ :< WT.CellElim e ->
      holes e
    _ :< WT.Let _ mxt e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' [mxt] (holes e2)
      S.union set1 set2
    _ :< WT.Hole h es ->
      S.insert h $ S.unions $ map holes es
    _ :< WT.ResourceType {} ->
      S.empty
    _ :< WT.Prim prim ->
      foldMap holes prim
    _ :< WT.Magic der ->
      foldMap holes der
    _ :< WT.Annotation _ annot e -> do
      let xs1 = holes e
      case annot of
        AN.Type t -> do
          let xs2 = holes t
          S.union xs1 xs2

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
holesCase (DT.Cons _ _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  S.unions $ holes' consArgs (holesDecisionTree tree) : map holes dataTerms ++ map holes dataTypes
