module Entity.WeakTerm.Holes (holes) where

import Control.Comonad.Cofree
import qualified Data.Set as S
import Entity.Binder
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
      holes' xts [t]
    _ :< WT.PiIntro _ xts e ->
      holes' xts [e]
    _ :< WT.PiElim e es ->
      S.unions $ map holes $ e : es
    _ :< WT.Sigma xts ->
      holes' xts []
    _ :< WT.SigmaIntro es ->
      S.unions $ map holes es
    _ :< WT.SigmaElim xts e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' xts [e2]
      S.union set1 set2
    _ :< WT.Let mxt e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' [mxt] [e2]
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
    _ :< WT.Match (e, t) patList -> do
      let xs1 = holes e
      let xs2 = holes t
      let xs3 = S.unions $ map (\((_, _, _, xts), body) -> holes' xts [body]) patList
      S.unions [xs1, xs2, xs3]

holes' :: [BinderF WT.WeakTerm] -> [WT.WeakTerm] -> S.Set HoleID
holes' binder es =
  case binder of
    [] ->
      S.unions $ map holes es
    ((_, _, t) : xts) -> do
      let set1 = holes t
      let set2 = holes' xts es
      S.union set1 set2
