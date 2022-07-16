module Entity.WeakTerm.Fill
  ( fill,
  )
where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe
import Entity.Binder
import Entity.HoleSubst
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.WeakTerm
import Entity.WeakTerm.Reduce
import Entity.WeakTerm.Subst
import Prelude hiding (lookup)

fill :: Context -> HoleSubst -> WeakTerm -> IO WeakTerm
fill ctx sub term =
  case term of
    _ :< WeakTermTau ->
      return term
    _ :< WeakTermVar {} ->
      return term
    _ :< WeakTermVarGlobal {} ->
      return term
    m :< WeakTermPi xts t -> do
      (xts', t') <- fill' ctx sub xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- fill' ctx sub (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          (xts', e') <- fill' ctx sub xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      e' <- fill ctx sub e
      es' <- mapM (fill ctx sub) es
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      (xts', _) <- fill' ctx sub xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (fill ctx sub) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- fill ctx sub e1
      (xts', e2') <- fill' ctx sub xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- fill ctx sub e1
      ([mxt'], e2') <- fill' ctx sub [mxt] e2
      return $ m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermPrim _ ->
      return term
    m :< WeakTermAster i es -> do
      es' <- mapM (fill ctx sub) es
      case lookup i sub of
        Just (xs, body)
          | length xs == length es -> do
            let varList = map Ident.toInt xs
            subst ctx (IntMap.fromList $ zip varList es') body >>= reduce ctx
          | otherwise ->
            error "Entity.WeakTerm.Fill (assertion failure; arity mismatch)"
        Nothing ->
          return $ m :< WeakTermAster i es'
    m :< WeakTermInt t x -> do
      t' <- fill ctx sub t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- fill ctx sub t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      return term
    _ :< WeakTermEnumIntro {} ->
      return term
    m :< WeakTermEnumElim (e, t) branchList -> do
      t' <- fill ctx sub t
      e' <- fill ctx sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (fill ctx sub) es
      return $ m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      e' <- fill ctx sub e
      t' <- fill ctx sub t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      der' <- mapM (fill ctx sub) der
      return $ m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (fill ctx sub) mSubject
      e' <- fill ctx sub e
      t' <- fill ctx sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- fill' ctx sub xts body
        return ((mPat, name, xts'), body')
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- fill ctx sub s
      e' <- fill ctx sub e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- fill ctx sub e
      return $ m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- fill ctx sub e
      return $ m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- fill ctx sub elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- fill ctx sub elemType
      elems' <- mapM (fill ctx sub) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- fill ctx sub subject
      elemType' <- fill ctx sub elemType
      array' <- fill ctx sub array
      index' <- fill ctx sub index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- fill ctx sub contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- fill ctx sub contentType
      content' <- fill ctx sub content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- fill ctx sub cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- fill ctx sub cell
      newValue' <- fill ctx sub newValue
      return $ m :< WeakTermCellWrite cell' newValue'
    _ :< WeakTermResourceType _ ->
      return term

fill' ::
  Context ->
  HoleSubst ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
fill' ctx sub binder e =
  case binder of
    [] -> do
      e' <- fill ctx sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      (xts', e') <- fill' ctx sub xts e
      t' <- fill ctx sub t
      return ((m, x, t') : xts', e')
