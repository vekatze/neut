module Entity.WeakTerm.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import qualified Data.Set as S
import Entity.Binder
import Entity.Ident
import Entity.LamKind
import Entity.WeakTerm

freeVars :: WeakTerm -> S.Set Ident
freeVars term =
  case term of
    _ :< WeakTermTau ->
      S.empty
    _ :< WeakTermVar x ->
      S.singleton x
    _ :< WeakTermVarGlobal {} ->
      S.empty
    _ :< WeakTermPi xts t ->
      freeVars' xts [t]
    _ :< WeakTermPiIntro k xts e ->
      freeVars' (catMaybes [fromLamKind k] ++ xts) [e]
    _ :< WeakTermPiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< WeakTermSigma xts ->
      freeVars' xts []
    _ :< WeakTermSigmaIntro es ->
      S.unions $ map freeVars es
    _ :< WeakTermSigmaElim xts e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' xts [e2]
      S.union set1 set2
    _ :< WeakTermLet mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' [mxt] [e2]
      S.union set1 set2
    _ :< WeakTermConst _ ->
      S.empty
    _ :< WeakTermAster _ ->
      S.empty
    _ :< WeakTermInt t _ ->
      freeVars t
    _ :< WeakTermFloat t _ ->
      freeVars t
    _ :< WeakTermEnum {} ->
      S.empty
    _ :< WeakTermEnumIntro {} ->
      S.empty
    _ :< WeakTermEnumElim (e, t) les -> do
      let xs = freeVars t
      let ys = freeVars e
      let zs = S.unions $ map (freeVars . snd) les
      S.unions [xs, ys, zs]
    _ :< WeakTermQuestion e t -> do
      let set1 = freeVars e
      let set2 = freeVars t
      S.union set1 set2
    _ :< WeakTermMagic der ->
      foldMap freeVars der
    _ :< WeakTermMatch mSubject (e, t) patList -> do
      let xs1 = S.unions $ map freeVars $ maybeToList mSubject
      let xs2 = freeVars e
      let xs3 = freeVars t
      let xs4 = S.unions $ map (\((_, _, xts), body) -> freeVars' xts [body]) patList
      S.unions [xs1, xs2, xs3, xs4]
    _ :< WeakTermNoema s e ->
      S.unions [freeVars s, freeVars e]
    _ :< WeakTermNoemaIntro s e ->
      S.insert s $ freeVars e
    _ :< WeakTermNoemaElim s e ->
      S.filter (/= s) $ freeVars e
    _ :< WeakTermArray elemType ->
      freeVars elemType
    _ :< WeakTermArrayIntro elemType elems ->
      S.unions $ freeVars elemType : map freeVars elems
    _ :< WeakTermArrayAccess subject elemType array index ->
      S.unions $ map freeVars [subject, elemType, array, index]
    _ :< WeakTermText ->
      S.empty
    _ :< WeakTermTextIntro _ ->
      S.empty
    _ :< WeakTermCell contentType ->
      freeVars contentType
    _ :< WeakTermCellIntro contentType content ->
      S.unions [freeVars contentType, freeVars content]
    _ :< WeakTermCellRead cell ->
      freeVars cell
    _ :< WeakTermCellWrite cell newValue ->
      S.unions [freeVars cell, freeVars newValue]

freeVars' :: [BinderF WeakTerm] -> [WeakTerm] -> S.Set Ident
freeVars' binder es =
  case binder of
    [] ->
      S.unions $ map freeVars es
    ((_, x, t) : xts) -> do
      let hs1 = freeVars t
      let hs2 = freeVars' xts es
      S.union hs1 $ S.filter (/= x) hs2
