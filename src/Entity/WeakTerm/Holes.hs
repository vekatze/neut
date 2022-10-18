module Entity.WeakTerm.Holes (holes) where

import Control.Comonad.Cofree
import qualified Data.Set as S
import Entity.Binder
import Entity.HoleID
import Entity.WeakTerm

holes :: WeakTerm -> S.Set HoleID
holes term =
  case term of
    _ :< WeakTermTau ->
      S.empty
    _ :< WeakTermVar {} ->
      S.empty
    _ :< WeakTermVarGlobal {} ->
      S.empty
    _ :< WeakTermPi xts t ->
      holes' xts [t]
    _ :< WeakTermPiIntro _ xts e ->
      holes' xts [e]
    _ :< WeakTermPiElim e es ->
      S.unions $ map holes $ e : es
    _ :< WeakTermSigma xts ->
      holes' xts []
    _ :< WeakTermSigmaIntro es ->
      S.unions $ map holes es
    _ :< WeakTermSigmaElim xts e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' xts [e2]
      S.union set1 set2
    _ :< WeakTermLet mxt e1 e2 -> do
      let set1 = holes e1
      let set2 = holes' [mxt] [e2]
      S.union set1 set2
    _ :< WeakTermAster h es ->
      S.insert h $ S.unions $ map holes es
    _ :< WeakTermPrim _ ->
      S.empty
    _ :< WeakTermInt t _ ->
      holes t
    _ :< WeakTermFloat t _ ->
      holes t
    _ :< WeakTermEnum {} ->
      S.empty
    _ :< WeakTermEnumIntro {} ->
      S.empty
    _ :< WeakTermEnumElim (e, t) les -> do
      let set1 = holes e
      let set2 = holes t
      let set3 = S.unions $ map (\(_, body) -> holes body) les
      S.unions [set1, set2, set3]
    _ :< WeakTermQuestion e t -> do
      let set1 = holes e
      let set2 = holes t
      S.union set1 set2
    _ :< WeakTermMagic der ->
      foldMap holes der
    _ :< WeakTermMatch (e, t) patList -> do
      let xs1 = holes e
      let xs2 = holes t
      let xs3 = S.unions $ map (\((_, _, _, xts), body) -> holes' xts [body]) patList
      S.unions [xs1, xs2, xs3]

holes' :: [BinderF WeakTerm] -> [WeakTerm] -> S.Set HoleID
holes' binder es =
  case binder of
    [] ->
      S.unions $ map holes es
    ((_, _, t) : xts) -> do
      let set1 = holes t
      let set2 = holes' xts es
      S.union set1 set2
