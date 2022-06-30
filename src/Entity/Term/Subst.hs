module Entity.Term.Subst (subst) where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Term

type SubstTerm =
  IntMap.IntMap Term

subst :: Axis -> SubstTerm -> Term -> IO Term
subst axis sub term =
  case term of
    (_ :< TermTau) ->
      return term
    (_ :< TermVar x)
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
        return e
      | otherwise ->
        return term
    (_ :< TermVarGlobal {}) ->
      return term
    (m :< TermPi xts t) -> do
      (xts', t') <- subst' axis sub xts t
      return (m :< TermPi xts' t')
    (m :< TermPiIntro kind xts e) -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- subst' axis sub (xt : xts) e
          return (m :< TermPiIntro (LamKindFix xt') xts' e')
        _ -> do
          (xts', e') <- subst' axis sub xts e
          return (m :< TermPiIntro kind xts' e')
    (m :< TermPiElim e es) -> do
      e' <- subst axis sub e
      es' <- mapM (subst axis sub) es
      return (m :< TermPiElim e' es')
    m :< TermSigma xts -> do
      (xts', _) <- subst' axis sub xts (m :< TermTau)
      return $ m :< TermSigma xts'
    m :< TermSigmaIntro es -> do
      es' <- mapM (subst axis sub) es
      return $ m :< TermSigmaIntro es'
    m :< TermSigmaElim xts e1 e2 -> do
      e1' <- subst axis sub e1
      (xts', e2') <- subst' axis sub xts e2
      return $ m :< TermSigmaElim xts' e1' e2'
    m :< TermLet mxt e1 e2 -> do
      e1' <- subst axis sub e1
      ([mxt'], e2') <- subst' axis sub [mxt] e2
      return $ m :< TermLet mxt' e1' e2'
    (_ :< TermConst _) ->
      return term
    (_ :< TermInt {}) ->
      return term
    (_ :< TermFloat {}) ->
      return term
    (_ :< TermEnum _) ->
      return term
    (_ :< TermEnumIntro _) ->
      return term
    (m :< TermEnumElim (e, t) branchList) -> do
      t' <- subst axis sub t
      e' <- subst axis sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst axis sub) es
      return (m :< TermEnumElim (e', t') (zip caseList es'))
    (m :< TermMagic der) -> do
      der' <- traverse (subst axis sub) der
      return (m :< TermMagic der')
    (m :< TermMatch mSubject (e, t) clauseList) -> do
      mSubject' <- mapM (subst axis sub) mSubject
      e' <- subst axis sub e
      t' <- subst axis sub t
      clauseList' <- forM clauseList $ \((mPat, name, xts), body) -> do
        (xts', body') <- subst' axis sub xts body
        return ((mPat, name, xts'), body')
      return (m :< TermMatch mSubject' (e', t') clauseList')
    m :< TermNoema s e -> do
      s' <- subst axis sub s
      e' <- subst axis sub e
      return $ m :< TermNoema s' e'
    m :< TermNoemaIntro s e -> do
      e' <- subst axis sub e
      return $ m :< TermNoemaIntro s e'
    m :< TermNoemaElim s e -> do
      e' <- subst axis sub e
      return $ m :< TermNoemaElim s e'
    _ :< TermArray _ ->
      return term
    m :< TermArrayIntro elemType elems -> do
      elems' <- mapM (subst axis sub) elems
      return $ m :< TermArrayIntro elemType elems'
    m :< TermArrayAccess subject elemType array index -> do
      subject' <- subst axis sub subject
      array' <- subst axis sub array
      index' <- subst axis sub index
      return $ m :< TermArrayAccess subject' elemType array' index'
    _ :< TermText ->
      return term
    _ :< TermTextIntro _ ->
      return term
    m :< TermCell contentType -> do
      contentType' <- subst axis sub contentType
      return $ m :< TermCell contentType'
    m :< TermCellIntro contentType content -> do
      contentType' <- subst axis sub contentType
      content' <- subst axis sub content
      return $ m :< TermCellIntro contentType' content'
    m :< TermCellRead cell -> do
      cell' <- subst axis sub cell
      return $ m :< TermCellRead cell'
    m :< TermCellWrite cell newValue -> do
      cell' <- subst axis sub cell
      newValue' <- subst axis sub newValue
      return $ m :< TermCellWrite cell' newValue'

subst' ::
  Axis ->
  SubstTerm ->
  [BinderF Term] ->
  Term ->
  IO ([BinderF Term], Term)
subst' axis sub binder e =
  case binder of
    [] -> do
      e' <- subst axis sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst axis sub t
      x' <- newIdentFromIdent axis x
      let sub' = IntMap.insert (Ident.toInt x) (m :< TermVar x') sub
      (xts', e') <- subst' axis sub' xts e
      return ((m, x', t') : xts', e')
