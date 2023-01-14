module Entity.WeakTerm.Subst
  ( subst,
    Context (..),
  )
where

import Context.Gensym
import Control.Comonad.Cofree
import qualified Data.IntMap as IntMap
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.WeakTerm as WT

subst :: Context m => WT.SubstWeakTerm -> WT.WeakTerm -> m WT.WeakTerm
subst sub term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var x
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          return e
      | otherwise ->
          return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi xts t -> do
      (xts', t') <- subst' sub xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- subst'' sub xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        _ -> do
          (xts', e') <- subst' sub xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.Data name es -> do
      es' <- mapM (subst sub) es
      return $ m :< WT.Data name es'
    m :< WT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (subst sub) dataArgs
      consArgs' <- mapM (subst sub) consArgs
      return $ m :< WT.DataIntro dataName consName disc dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst''' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Sigma xts -> do
      (xts', _) <- subst' sub xts (m :< WT.Tau)
      return $ m :< WT.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM (subst sub) es
      return $ m :< WT.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- subst sub e1
      (xts', e2') <- subst' sub xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< WT.Noema t -> do
      t' <- subst sub t
      return $ m :< WT.Noema t'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- subst sub e1
      (mxt', _, e2') <- subst'' sub mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (subst sub) prim
      return $ m :< WT.Prim prim'
    m :< WT.Hole holeID args -> do
      args' <- mapM (subst sub) args
      return $ m :< WT.Hole holeID args'
    m :< WT.Question e t -> do
      e' <- subst sub e
      t' <- subst sub t
      return $ m :< WT.Question e' t'
    m :< WT.Magic der -> do
      der' <- mapM (subst sub) der
      return $ m :< WT.Magic der'

subst' ::
  Context m =>
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
subst' sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
      (xts', e') <- subst' sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  Context m =>
  WT.SubstWeakTerm ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
subst'' sub (m, x, t) binder e = do
  t' <- subst sub t
  x' <- newIdentFromIdent x
  let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
  (xts', e') <- subst' sub' binder e
  return ((m, x, t'), xts', e')

subst''' ::
  Context m =>
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
subst''' sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (m :< WT.Var x') sub
      (xts', e') <- subst''' sub' xts decisionTree
      return ((m, x', t') : xts', e')

substDecisionTree ::
  Context m =>
  WT.SubstWeakTerm ->
  DT.DecisionTree WT.WeakTerm ->
  m (DT.DecisionTree WT.WeakTerm)
substDecisionTree sub tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- subst sub e
      let xs' = filter (\x -> Ident.toInt x `IntMap.notMember` sub) xs
      return $ DT.Leaf xs' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- subst sub cursor
      caseList' <- substCaseList sub caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

substCaseList ::
  Context m =>
  WT.SubstWeakTerm ->
  DT.CaseList WT.WeakTerm ->
  m (DT.CaseList WT.WeakTerm)
substCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree sub fallbackClause
  clauseList' <- mapM (substCase sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  Context m =>
  WT.SubstWeakTerm ->
  DT.Case WT.WeakTerm ->
  m (DT.Case WT.WeakTerm)
substCase sub (DT.Cons dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM (subst sub) dataTerms
  dataTypes' <- mapM (subst sub) dataTypes
  (consArgs', tree') <- subst''' sub consArgs tree
  return $ DT.Cons dd disc (zip dataTerms' dataTypes') consArgs' tree'
