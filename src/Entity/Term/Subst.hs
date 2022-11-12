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
import qualified Entity.LamKind as LK
import qualified Entity.Term as TM

type SubstTerm =
  IntMap.IntMap TM.Term

class MonadFail m => Context m where
  newIdentFromIdent :: Ident -> m Ident

subst :: Context m => SubstTerm -> TM.Term -> m TM.Term
subst sub term =
  case term of
    (_ :< TM.Tau) ->
      return term
    (_ :< TM.Var x)
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          return e
      | otherwise ->
          return term
    (_ :< TM.VarGlobal {}) ->
      return term
    (m :< TM.Pi xts t) -> do
      (xts', t') <- subst' sub xts t
      return (m :< TM.Pi xts' t')
    (m :< TM.PiIntro kind xts e) -> do
      case kind of
        LK.Fix xt -> do
          (xt' : xts', e') <- subst' sub (xt : xts) e
          return (m :< TM.PiIntro (LK.Fix xt') xts' e')
        _ -> do
          (xts', e') <- subst' sub xts e
          return (m :< TM.PiIntro kind xts' e')
    (m :< TM.PiElim e es) -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return (m :< TM.PiElim e' es')
    m :< TM.Sigma xts -> do
      (xts', _) <- subst' sub xts (m :< TM.Tau)
      return $ m :< TM.Sigma xts'
    m :< TM.SigmaIntro es -> do
      es' <- mapM (subst sub) es
      return $ m :< TM.SigmaIntro es'
    m :< TM.SigmaElim xts e1 e2 -> do
      e1' <- subst sub e1
      (xts', e2') <- subst' sub xts e2
      return $ m :< TM.SigmaElim xts' e1' e2'
    m :< TM.Let mxt e1 e2 -> do
      e1' <- subst sub e1
      ([mxt'], e2') <- subst' sub [mxt] e2
      return $ m :< TM.Let mxt' e1' e2'
    (_ :< TM.Prim _) ->
      return term
    (_ :< TM.Enum {}) ->
      return term
    (_ :< TM.EnumIntro {}) ->
      return term
    (m :< TM.EnumElim (e, t) branchList) -> do
      t' <- subst sub t
      e' <- subst sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (subst sub) es
      return (m :< TM.EnumElim (e', t') (zip caseList es'))
    (m :< TM.Magic der) -> do
      der' <- traverse (subst sub) der
      return (m :< TM.Magic der')
    (m :< TM.Match (e, t) clauseList) -> do
      e' <- subst sub e
      t' <- subst sub t
      clauseList' <- forM clauseList $ \((mPat, name, arity, xts), body) -> do
        (xts', body') <- subst' sub xts body
        return ((mPat, name, arity, xts'), body')
      return (m :< TM.Match (e', t') clauseList')

subst' ::
  Context m =>
  SubstTerm ->
  [BinderF TM.Term] ->
  TM.Term ->
  m ([BinderF TM.Term], TM.Term)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< TM.Var x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')
