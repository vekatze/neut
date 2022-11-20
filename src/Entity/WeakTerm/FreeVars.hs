module Entity.WeakTerm.FreeVars (freeVars) where

import Control.Comonad.Cofree
import Data.Maybe
import qualified Data.Set as S
import Entity.Binder
import Entity.Ident
import Entity.LamKind
import qualified Entity.WeakTerm as WT

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
      freeVars' xts [t]
    _ :< WT.PiIntro k xts e ->
      freeVars' (catMaybes [fromLamKind k] ++ xts) [e]
    _ :< WT.PiElim e es -> do
      let xs = freeVars e
      let ys = S.unions $ map freeVars es
      S.union xs ys
    _ :< WT.Sigma xts ->
      freeVars' xts []
    _ :< WT.SigmaIntro es ->
      S.unions $ map freeVars es
    _ :< WT.SigmaElim xts e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' xts [e2]
      S.union set1 set2
    _ :< WT.Let mxt e1 e2 -> do
      let set1 = freeVars e1
      let set2 = freeVars' [mxt] [e2]
      S.union set1 set2
    _ :< WT.Prim prim ->
      foldMap freeVars prim
    _ :< WT.Aster _ es ->
      S.unions $ map freeVars es
    _ :< WT.Enum {} ->
      S.empty
    _ :< WT.EnumIntro {} ->
      S.empty
    _ :< WT.EnumElim (e, t) les -> do
      let xs = freeVars t
      let ys = freeVars e
      let zs = S.unions $ map (freeVars . snd) les
      S.unions [xs, ys, zs]
    _ :< WT.Question e t -> do
      let set1 = freeVars e
      let set2 = freeVars t
      S.union set1 set2
    _ :< WT.Magic der ->
      foldMap freeVars der
    _ :< WT.DataElim oets patList -> do
      let xs1 = freeVars e
      let xs2 = freeVars t
      let xs3 = S.unions $ map (\((_, _, _, xts), body) -> freeVars' xts [body]) patList
      S.unions [xs1, xs2, xs3]

freeVars' :: [BinderF WT.WeakTerm] -> [WT.WeakTerm] -> S.Set Ident
freeVars' binder es =
  case binder of
    [] ->
      S.unions $ map freeVars es
    ((_, x, t) : xts) -> do
      let hs1 = freeVars t
      let hs2 = freeVars' xts es
      S.union hs1 $ S.filter (/= x) hs2
