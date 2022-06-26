module Entity.WeakTerm.Subst (subst) where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Basic
import Entity.Global
import Entity.WeakTerm

subst :: SubstWeakTerm -> WeakTerm -> IO WeakTerm
subst sub term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar x
      | Just e <- IntMap.lookup (asInt x) sub ->
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
          (xt' : xts', e') <- subst' sub (xt : xts) e
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
      ([mxt'], e2') <- subst' sub [mxt] e2
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
      t' <- subst sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- subst sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro _ ->
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
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (subst sub) mSubject
      e' <- subst sub e
      t' <- subst sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- subst' sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- subst sub s
      e' <- subst sub e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- subst sub e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- subst sub e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- subst sub elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- subst sub elemType
      elems' <- mapM (subst sub) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- subst sub subject
      elemType' <- subst sub elemType
      array' <- subst sub array
      index' <- subst sub index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- subst sub contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- subst sub contentType
      content' <- subst sub content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- subst sub cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- subst sub cell
      newValue' <- subst sub newValue
      return $ m :< WeakTermCellWrite cell' newValue'

subst' ::
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (asInt x) (m :< WeakTermVar x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')
