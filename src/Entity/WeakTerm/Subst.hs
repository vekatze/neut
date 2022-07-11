module Entity.WeakTerm.Subst (subst) where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm

subst :: Context -> SubstWeakTerm -> WeakTerm -> IO WeakTerm
subst ctx sub term =
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
      (xts', t') <- subst' ctx sub xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- subst' ctx sub (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          (xts', e') <- subst' ctx sub xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      e' <- subst ctx sub e
      es' <- mapM (subst ctx sub) es
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- subst' ctx sub xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (subst ctx sub) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- subst ctx sub e1
      (xts', e2') <- subst' ctx sub xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- subst ctx sub e1
      ([mxt'], e2') <- subst' ctx sub [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermPrim _ ->
      return term
    _ :< WeakTermAster x ->
      case IntMap.lookup x sub of
        Nothing ->
          return term
        Just e2 ->
          return e2
    m :< WeakTermInt t x -> do
      t' <- subst ctx sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- subst ctx sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro {} ->
      return term
    m :< WeakTermEnumElim (e, t) branchList -> do
      t' <- subst ctx sub t
      e' <- subst ctx sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst ctx sub) es
      return $ m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      e' <- subst ctx sub e
      t' <- subst ctx sub t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- mapM (subst ctx sub) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (subst ctx sub) mSubject
      e' <- subst ctx sub e
      t' <- subst ctx sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- subst' ctx sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- subst ctx sub s
      e' <- subst ctx sub e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- subst ctx sub e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- subst ctx sub e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- subst ctx sub elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- subst ctx sub elemType
      elems' <- mapM (subst ctx sub) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- subst ctx sub subject
      elemType' <- subst ctx sub elemType
      array' <- subst ctx sub array
      index' <- subst ctx sub index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- subst ctx sub contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- subst ctx sub contentType
      content' <- subst ctx sub content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- subst ctx sub cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- subst ctx sub cell
      newValue' <- subst ctx sub newValue
      return $ m :< WeakTermCellWrite cell' newValue'
    _ :< WeakTermResourceType _ ->
      return term

subst' ::
  Context ->
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
subst' ctx sub binder e =
  case binder of
    [] -> do
      e' <- subst ctx sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst ctx sub t
      x' <- newIdentFromIdent ctx x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WeakTermVar x') sub
      (xts', e') <- subst' ctx sub' xts e
      return ((m, x', t') : xts', e')
