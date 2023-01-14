module Entity.Term.Subst
  ( subst,
    Context (..),
  )
where

import Control.Comonad.Cofree
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.DecisionTree as DT
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
    m :< TM.Data name es -> do
      es' <- mapM (subst sub) es
      return $ m :< TM.Data name es'
    m :< TM.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (subst sub) dataArgs
      consArgs' <- mapM (subst sub) consArgs
      return $ m :< TM.DataIntro dataName consName disc dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst'' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
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
    m :< TM.Noema t -> do
      t' <- subst sub t
      return $ m :< TM.Noema t'
    (_ :< TM.Prim _) ->
      return term
    (m :< TM.Magic der) -> do
      der' <- traverse (subst sub) der
      return (m :< TM.Magic der')

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

subst'' ::
  Context m =>
  SubstTerm ->
  [BinderF TM.Term] ->
  DT.DecisionTree TM.Term ->
  m ([BinderF TM.Term], DT.DecisionTree TM.Term)
subst'' sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< TM.Var x') sub
      (xts', e') <- subst'' sub' xts decisionTree
      return ((m, x', t') : xts', e')

substDecisionTree ::
  Context m =>
  SubstTerm ->
  DT.DecisionTree TM.Term ->
  m (DT.DecisionTree TM.Term)
substDecisionTree sub tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- subst sub e
      let xs' = filter (\x -> Ident.toInt x `IntMap.notMember` sub) xs
      return $ DT.Leaf xs' e'
    DT.Unreachable ->
      return tree
    DT.Switch cursor caseList -> do
      caseList' <- substCaseList sub caseList
      return $ DT.Switch cursor caseList'

substCaseList ::
  Context m =>
  SubstTerm ->
  DT.CaseList TM.Term ->
  m (DT.CaseList TM.Term)
substCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree sub fallbackClause
  clauseList' <- mapM (substCase sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Context m =>
  SubstTerm ->
  DT.Case TM.Term ->
  m (DT.Case TM.Term)
substCase sub (DT.Cons dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM (subst sub) dataTerms
  dataTypes' <- mapM (subst sub) dataTypes
  (consArgs', tree') <- subst'' sub consArgs tree
  return $ DT.Cons dd disc (zip dataTerms' dataTypes') consArgs' tree'
