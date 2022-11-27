module Entity.WeakTerm.Fill
  ( fill,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Maybe
import Entity.Binder
import qualified Entity.DecisionTree as DT
import Entity.HoleSubst
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.WeakTerm as WT
import Entity.WeakTerm.Reduce
import Entity.WeakTerm.Subst
import Prelude hiding (lookup)

fill :: Context m => HoleSubst -> WT.WeakTerm -> m WT.WeakTerm
fill sub term =
  case term of
    _ :< WT.Tau ->
      return term
    _ :< WT.Var {} ->
      return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi xts t -> do
      (xts', t') <- fill' sub xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- fill'' sub xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        _ -> do
          (xts', e') <- fill' sub xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      e' <- fill sub e
      es' <- mapM (fill sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.Data name es -> do
      es' <- mapM (fill sub) es
      return $ m :< WT.Data name es'
    m :< WT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (fill sub) dataArgs
      consArgs' <- mapM (fill sub) consArgs
      return $ m :< WT.DataIntro dataName consName disc dataArgs' consArgs'
    m :< WT.DataElim oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- fill''' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim (zip3 os' es' ts') decisionTree'
    m :< WT.Sigma xts -> do
      (xts', _) <- fill' sub xts (m :< WT.Tau)
      return $ m :< WT.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM (fill sub) es
      return $ m :< WT.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- fill sub e1
      (xts', e2') <- fill' sub xts e2
      return $ m :< WT.SigmaElim xts' e1' e2'
    m :< WT.Let mxt e1 e2 -> do
      e1' <- fill sub e1
      (mxt', _, e2') <- fill'' sub mxt [] e2
      return $ m :< WT.Let mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fill sub) prim
      return $ m :< WT.Prim prim'
    m :< WT.Aster i es -> do
      es' <- mapM (fill sub) es
      case lookup i sub of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              subst (IntMap.fromList $ zip varList es') body >>= reduce
          | otherwise ->
              error "Entity.WeakTerm.Fill (assertion failure; arity mismatch)"
        Nothing ->
          return $ m :< WT.Aster i es'
    _ :< WT.Enum {} ->
      return term
    _ :< WT.EnumIntro {} ->
      return term
    m :< WT.EnumElim (e, t) branchList -> do
      t' <- fill sub t
      e' <- fill sub e
      let (caseList, es) = unzip branchList
      es' <- mapM (fill sub) es
      return $ m :< WT.EnumElim (e', t') (zip caseList es')
    m :< WT.Question e t -> do
      e' <- fill sub e
      t' <- fill sub t
      return $ m :< WT.Question e' t'
    m :< WT.Magic der -> do
      der' <- mapM (fill sub) der
      return $ m :< WT.Magic der'

fill' ::
  Context m =>
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
fill' sub binder e =
  case binder of
    [] -> do
      e' <- fill sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      (xts', e') <- fill' sub xts e
      t' <- fill sub t
      return ((m, x, t') : xts', e')

fill'' ::
  Context m =>
  HoleSubst ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' sub (m, x, t) binder e = do
  (xts', e') <- fill' sub binder e
  t' <- fill sub t
  return ((m, x, t'), xts', e')

fill''' ::
  Context m =>
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
fill''' sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- fillDecisionTree sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- fill sub t
      (xts', e') <- fill''' sub xts decisionTree
      return ((m, x, t') : xts', e')

fillDecisionTree ::
  Context m =>
  HoleSubst ->
  DT.DecisionTree WT.WeakTerm ->
  m (DT.DecisionTree WT.WeakTerm)
fillDecisionTree sub tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- fill sub e
      return $ DT.Leaf xs e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- fill sub cursor
      caseList' <- fillCaseList sub caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

fillCaseList ::
  Context m =>
  HoleSubst ->
  DT.CaseList WT.WeakTerm ->
  m (DT.CaseList WT.WeakTerm)
fillCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree sub fallbackClause
  clauseList' <- mapM (fillCase sub) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  Context m =>
  HoleSubst ->
  DT.Case WT.WeakTerm ->
  m (DT.Case WT.WeakTerm)
fillCase sub (DT.Cons dd disc dataArgs consArgs tree) = do
  (xts', tree') <- fill''' sub (dataArgs ++ consArgs) tree
  let len = length dataArgs
  let dataArgs' = take len xts'
  let consArgs' = drop len xts'
  return $ DT.Cons dd disc dataArgs' consArgs' tree'
