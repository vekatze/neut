module Entity.WeakTerm.Fill
  ( fill,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe
import Entity.Binder
import Entity.HoleSubst
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.WeakTerm as WT
import Entity.WeakTerm.Reduce
import Entity.WeakTerm.Subst
import Prelude hiding (lookup)

fill :: Context m => HoleSubst -> WT.WeakTerm -> m WT.WeakTerm
fill sub term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi xts t -> do
      (xts', t') <- fill' sub xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- fill'' sub xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        _ -> do
          (xts', e') <- fill' sub xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      e' <- fill sub e
      es' <- mapM (fill sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.Sigma xts -> do
      (xts', _) <- fill' sub xts (m :< WT.Tau)
      return $ m :< WT.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM (fill sub) es
      return $ m :< WT.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- fill sub e1
      (xts', e2') <- fill' sub xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< WT.Let mxt e1 e2 -> do
      e1' <- fill sub e1
      (mxt', _, e2') <- fill'' sub mxt [] e2
      -- ([mxt'], e2') <- fill' sub [mxt] e2
      return $ m :< WT.Let mxt' e1' e2'
    _ :< WT.Prim _ ->
      return term
    m :< WT.Aster i es -> do
      es' <- mapM (fill sub) es
      case lookup i sub of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              subst (IntMap.fromList $ zip varList es') body >>= reduce
          | otherwise ->
              error "Entity.WeakTerm.Fill (assertion failure; arity mismatch)"
        Nothing ->
          return $ m :< WT.Aster i es'
    m :< WT.Int t x -> do
      t' <- fill sub t
      return $ m :< WT.Int t' x
    m :< WT.Float t x -> do
      t' <- fill sub t
      return $ m :< WT.Float t' x
    _ :< WT.Enum {} ->
      return term
    _ :< WT.EnumIntro {} ->
      return term
    m :< WT.EnumElim (e, t) branchList -> do
      t' <- fill sub t
      e' <- fill sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (fill sub) es
      return $ m :< WT.EnumElim (e', t') (zip caseList es')
    m :< WT.Question e t -> do
      e' <- fill sub e
      t' <- fill sub t
      return $ m :< WT.Question e' t'
    m :< WT.Magic der -> do
      der' <- mapM (fill sub) der
      return $ m :< WT.Magic der'
    m :< WT.Match (e, t) clauseList -> do
      e' <- fill sub e
      t' <- fill sub t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        (xts', body') <- fill' sub xts body
        return ((mPat, name, arity, xts'), body')
      return $ m :< WT.Match (e', t') clauseList'

fill' ::
  Context m =>
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
fill' sub binder e =
  case binder of
    [] -> do
      e' <- fill sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      (xts', e') <- fill' sub xts e
      t' <- fill sub t
      return ((m, x, t') : xts', e')

fill'' ::
  Context m =>
  HoleSubst ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' sub (m, x, t) binder e = do
  (xts', e') <- fill' sub binder e
  t' <- fill sub t
  return ((m, x, t'), xts', e')
