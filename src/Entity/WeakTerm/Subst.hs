module Entity.WeakTerm.Subst (subst) where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm

subst :: Axis -> SubstWeakTerm -> WeakTerm -> IO WeakTerm
subst axis sub term =
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
      (xts', t') <- subst' axis sub xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- subst' axis sub (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          (xts', e') <- subst' axis sub xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      e' <- subst axis sub e
      es' <- mapM (subst axis sub) es
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- subst' axis sub xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (subst axis sub) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- subst axis sub e1
      (xts', e2') <- subst' axis sub xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- subst axis sub e1
      ([mxt'], e2') <- subst' axis sub [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    _ :< WeakTermAster x ->
      case IntMap.lookup x sub of
        Nothing ->
          return term
        Just e2 ->
          return e2
    m :< WeakTermInt t x -> do
      t' <- subst axis sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- subst axis sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro {} ->
      return term
    m :< WeakTermEnumElim (e, t) branchList -> do
      t' <- subst axis sub t
      e' <- subst axis sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst axis sub) es
      return $ m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      e' <- subst axis sub e
      t' <- subst axis sub t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- mapM (subst axis sub) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (subst axis sub) mSubject
      e' <- subst axis sub e
      t' <- subst axis sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- subst' axis sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- subst axis sub s
      e' <- subst axis sub e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- subst axis sub e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- subst axis sub e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- subst axis sub elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- subst axis sub elemType
      elems' <- mapM (subst axis sub) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- subst axis sub subject
      elemType' <- subst axis sub elemType
      array' <- subst axis sub array
      index' <- subst axis sub index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- subst axis sub contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- subst axis sub contentType
      content' <- subst axis sub content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- subst axis sub cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- subst axis sub cell
      newValue' <- subst axis sub newValue
      return $ m :< WeakTermCellWrite cell' newValue'

subst' ::
  Axis ->
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
subst' axis sub binder e =
  case binder of
    [] -> do
      e' <- subst axis sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst axis sub t
      x' <- newIdentFromIdent axis x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WeakTermVar x') sub
      (xts', e') <- subst' axis sub' xts e
      return ((m, x', t') : xts', e')
