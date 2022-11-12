module Entity.WeakTerm.Subst
  ( subst,
    Context (..),
  )
where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.WeakTerm as WT

subst :: Context m => WT.SubstWeakTerm -> WT.WeakTerm -> m WT.WeakTerm
subst sub term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var x
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          return e
      | otherwise ->
          return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi xts t -> do
      (xts', t') <- subst' sub xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- subst'' sub xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        _ -> do
          (xts', e') <- subst' sub xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.Sigma xts -> do
      (xts', _) <- subst' sub xts (m :< WT.Tau)
      return $ m :< WT.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM (subst sub) es
      return $ m :< WT.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- subst sub e1
      (xts', e2') <- subst' sub xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< WT.Let mxt e1 e2 -> do
      e1' <- subst sub e1
      (mxt', _, e2') <- subst'' sub mxt [] e2
      return $ m :< WT.Let mxt' e1' e2'
    _ :< WT.Prim _ ->
      return term
    _ :< WT.Aster {} ->
      return term
    m :< WT.Int t x -> do
      t' <- subst sub t
      return $ m :< WT.Int t' x
    m :< WT.Float t x -> do
      t' <- subst sub t
      return $ m :< WT.Float t' x
    _ :< WT.Enum {} ->
      return term
    _ :< WT.EnumIntro {} ->
      return term
    m :< WT.EnumElim (e, t) branchList -> do
      t' <- subst sub t
      e' <- subst sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst sub) es
      return $ m :< WT.EnumElim (e', t') (zip caseList es')
    m :< WT.Question e t -> do
      e' <- subst sub e
      t' <- subst sub t
      return $ m :< WT.Question e' t'
    m :< WT.Magic der -> do
      der' <- mapM (subst sub) der
      return $ m :< WT.Magic der'
    m :< WT.Match (e, t) clauseList -> do
      e' <- subst sub e
      t' <- subst sub t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        (xts', body') <- subst' sub xts body
        return ((mPat, name, arity, xts'), body')
      return $ m :< WT.Match (e', t') clauseList'

subst' ::
  Context m =>
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  Context m =>
  WT.SubstWeakTerm ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
subst'' sub (m, x, t) binder e = do
  t' <- subst sub t
  x' <- newIdentFromIdent x
  let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
  (xts', e') <- subst' sub' binder e
  return ((m, x, t'), xts', e')

-- case binder of
--   [] -> do
--     e' <- subst sub e
--     return ([], e')
--   ((m, x, t) : xts) -> do
--     t' <- subst sub t
--     x' <- newIdentFromIdent x
--     let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
--     (xts', e') <- subst' sub' xts e
--     return ((m, x', t') : xts', e')
