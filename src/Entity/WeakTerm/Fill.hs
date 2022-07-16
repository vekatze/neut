module Entity.WeakTerm.Fill
  ( fill,
  )
where

import Control.Comonad.Cofree
import Data.Maybe
import Entity.Binder
import Entity.HoleSubst
import Entity.LamKind
import Entity.WeakTerm
import Prelude hiding (lookup)

fill :: HoleSubst -> WeakTerm -> WeakTerm
fill sub term =
  case term of
    _ :< WeakTermTau ->
      term
    _ :< WeakTermVar {} ->
      term
    _ :< WeakTermVarGlobal {} ->
      term
    m :< WeakTermPi xts t -> do
      let (xts', t') = fill' sub xts t
      m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          let (xt' : xts', e') = fill' sub (xt : xts) e
          m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        _ -> do
          let (xts', e') = fill' sub xts e
          m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      let e' = fill sub e
      let es' = map (fill sub) es
      m :< WeakTermPiElim e' es'
    m :< WeakTermSigma xts -> do
      let (xts', _) = fill' sub xts (m :< WeakTermTau)
      m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      let es' = map (fill sub) es
      m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      let e1' = fill sub e1
      let (xts', e2') = fill' sub xts e2
      m :< WeakTermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      let e1' = fill sub e1
      let ([mxt'], e2') = fill' sub [mxt] e2
      m :< WeakTermLet mxt' e1' e2'
    _ :< WeakTermPrim _ ->
      term
    m :< WeakTermAster i es -> do
      let es' = map (fill sub) es
      case lookup i sub of
        Just e ->
          m :< WeakTermPiElim e es'
        Nothing ->
          m :< WeakTermAster i es'
    m :< WeakTermInt t x -> do
      let t' = fill sub t
      m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      let t' = fill sub t
      m :< WeakTermFloat t' x
    _ :< WeakTermEnum {} ->
      term
    _ :< WeakTermEnumIntro {} ->
      term
    m :< WeakTermEnumElim (e, t) branchList -> do
      let t' = fill sub t
      let e' = fill sub e
      let (caseList, es) = unzip branchList
      let es' = map (fill sub) es
      m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< WeakTermQuestion e t -> do
      let e' = fill sub e
      let t' = fill sub t
      m :< WeakTermQuestion e' t'
    m :< WeakTermMagic der -> do
      let der' = fmap (fill sub) der
      m :< WeakTermMagic der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      let mSubject' = fmap (fill sub) mSubject
      let e' = fill sub e
      let t' = fill sub t
      let clauseList' = flip map clauseList $ \((mPat, name, xts), body) -> do
            let (xts', body') = fill' sub xts body
            ((mPat, name, xts'), body')
      m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      let s' = fill sub s
      let e' = fill sub e
      m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      let e' = fill sub e
      m :< WeakTermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      let e' = fill sub e
      m :< WeakTermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      let elemType' = fill sub elemType
      m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      let elemType' = fill sub elemType
      let elems' = map (fill sub) elems
      m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      let subject' = fill sub subject
      let elemType' = fill sub elemType
      let array' = fill sub array
      let index' = fill sub index
      m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      term
    _ :< WeakTermTextIntro _ ->
      term
    m :< WeakTermCell contentType -> do
      let contentType' = fill sub contentType
      m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      let contentType' = fill sub contentType
      let content' = fill sub content
      m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      let cell' = fill sub cell
      m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      let cell' = fill sub cell
      let newValue' = fill sub newValue
      m :< WeakTermCellWrite cell' newValue'
    _ :< WeakTermResourceType _ ->
      term

fill' ::
  HoleSubst ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  ([BinderF WeakTerm], WeakTerm)
fill' sub binder e =
  case binder of
    [] ->
      ([], fill sub e)
    ((m, x, t) : xts) -> do
      let (xts', e') = fill' sub xts e
      ((m, x, fill sub t) : xts', e')
