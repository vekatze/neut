module Entity.Term.Subst
  ( subst,
    Context (..),
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Entity.Binder
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Term

type SubstTerm =
  IntMap.IntMap Term

class MonadFail m => Context m where
  newIdentFromIdent :: Ident -> m Ident

subst :: Context m => SubstTerm -> Term -> m Term
subst sub term =
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
      (xts', t') <- subst' sub xts t
      return (m :< TermPi xts' t')
    (m :< TermPiIntro kind xts e) -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- subst' sub (xt : xts) e
          return (m :< TermPiIntro (LamKindFix xt') xts' e')
        _ -> do
          (xts', e') <- subst' sub xts e
          return (m :< TermPiIntro kind xts' e')
    (m :< TermPiElim e es) -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return (m :< TermPiElim e' es')
    m :< TermSigma xts -> do
      (xts', _) <- subst' sub xts (m :< TermTau)
      return $ m :< TermSigma xts'
    m :< TermSigmaIntro es -> do
      es' <- mapM (subst sub) es
      return $ m :< TermSigmaIntro es'
    m :< TermSigmaElim xts e1 e2 -> do
      e1' <- subst sub e1
      (xts', e2') <- subst' sub xts e2
      return $ m :< TermSigmaElim xts' e1' e2'
    m :< TermLet mxt e1 e2 -> do
      e1' <- subst sub e1
      ([mxt'], e2') <- subst' sub [mxt] e2
      return $ m :< TermLet mxt' e1' e2'
    (_ :< TermPrim _) ->
      return term
    (_ :< TermInt {}) ->
      return term
    (_ :< TermFloat {}) ->
      return term
    (_ :< TermEnum {}) ->
      return term
    (_ :< TermEnumIntro {}) ->
      return term
    (m :< TermEnumElim (e, t) branchList) -> do
      t' <- subst sub t
      e' <- subst sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst sub) es
      return (m :< TermEnumElim (e', t') (zip caseList es'))
    (m :< TermMagic der) -> do
      der' <- traverse (subst sub) der
      return (m :< TermMagic der')
    (m :< TermMatch (e, t) clauseList) -> do
      e' <- subst sub e
      t' <- subst sub t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        (xts', body') <- subst' sub xts body
        return ((mPat, name, arity, xts'), body')
      return (m :< TermMatch (e', t') clauseList')

subst' ::
  Context m =>
  SubstTerm ->
  [BinderF Term] ->
  Term ->
  m ([BinderF Term], Term)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< TermVar x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')
