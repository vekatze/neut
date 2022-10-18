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
import Entity.LamKind
import Entity.WeakTerm

subst :: Context m => SubstWeakTerm -> WeakTerm -> m WeakTerm
subst sub term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar x
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          return e
      | otherwise ->
          return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- subst' sub xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt', xts', e') <- subst'' sub xt xts e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          (xts', e') <- subst' sub xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- subst' sub xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (subst sub) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- subst sub e1
      (xts', e2') <- subst' sub xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- subst sub e1
      (mxt', _, e2') <- subst'' sub mxt [] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermPrim _ ->
      return term
    _ :< WeakTermAster {} ->
      return term
    m :< WeakTermInt t x -> do
      t' <- subst sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- subst sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro {} ->
      return term
    m :< WeakTermEnumElim (e, t) branchList -> do
      t' <- subst sub t
      e' <- subst sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst sub) es
      return $ m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      e' <- subst sub e
      t' <- subst sub t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- mapM (subst sub) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch (e, t) clauseList -> do
      e' <- subst sub e
      t' <- subst sub t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        (xts', body') <- subst' sub xts body
        return ((mPat, name, arity, xts'), body')
      return $ m :< WeakTermMatch (e', t') clauseList'

subst' ::
  Context m =>
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  m ([BinderF WeakTerm], WeakTerm)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WeakTermVar x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  Context m =>
  SubstWeakTerm ->
  BinderF WeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  m (BinderF WeakTerm, [BinderF WeakTerm], WeakTerm)
subst'' sub (m, x, t) binder e = do
  t' <- subst sub t
  x' <- newIdentFromIdent x
  let sub' = IntMap.insert (Ident.toInt x) (m :< WeakTermVar x') sub
  (xts', e') <- subst' sub' binder e
  return ((m, x, t'), xts', e')

-- case binder of
--   [] -> do
--     e' <- subst sub e
--     return ([], e')
--   ((m, x, t) : xts) -> do
--     t' <- subst sub t
--     x' <- newIdentFromIdent x
--     let sub' = IntMap.insert (Ident.toInt x) (m :< WeakTermVar x') sub
--     (xts', e') <- subst' sub' xts e
--     return ((m, x', t') : xts', e')
