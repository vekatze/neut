module Scene.WeakTerm.Fill
  ( fill,
  )
where

import Context.App
import Control.Comonad.Cofree
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.Maybe
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.HoleSubst
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.WeakTerm qualified as WT
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst
import Prelude hiding (lookup)

fill :: HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
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
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (fill sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- fill''' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Noema t -> do
      t' <- fill sub t
      return $ m :< WT.Noema t'
    m :< WT.Cell t -> do
      t' <- fill sub t
      return $ m :< WT.Cell t'
    m :< WT.CellIntro e -> do
      e' <- fill sub e
      return $ m :< WT.CellIntro e'
    m :< WT.CellElim e -> do
      e' <- fill sub e
      return $ m :< WT.CellElim e'
    m :< WT.Let opacity mxt e1 e2 -> do
      e1' <- fill sub e1
      (mxt', _, e2') <- fill'' sub mxt [] e2
      return $ m :< WT.Let opacity mxt' e1' e2'
    m :< WT.Prim prim -> do
      prim' <- mapM (fill sub) prim
      return $ m :< WT.Prim prim'
    m :< WT.Hole i es -> do
      es' <- mapM (fill sub) es
      case lookup i sub of
        Just (xs, body)
          | length xs == length es -> do
              let varList = map Ident.toInt xs
              subst (IntMap.fromList $ zip varList es') body >>= reduce
          | otherwise ->
              error "Entity.WeakTerm.Fill (assertion failure; arity mismatch)"
        Nothing ->
          return $ m :< WT.Hole i es'
    _ :< WT.ResourceType {} ->
      return term
    m :< WT.Magic der -> do
      der' <- mapM (fill sub) der
      return $ m :< WT.Magic der'

fill' ::
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
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
  HoleSubst ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
fill'' sub (m, x, t) binder e = do
  (xts', e') <- fill' sub binder e
  t' <- fill sub t
  return ((m, x, t'), xts', e')

fill''' ::
  HoleSubst ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
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
  HoleSubst ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
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
  HoleSubst ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
fillCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- fillDecisionTree sub fallbackClause
  clauseList' <- mapM (fillCase sub) clauseList
  return (fallbackClause', clauseList')

fillCase ::
  HoleSubst ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
fillCase sub (DT.Cons dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM (fill sub) dataTerms
  dataTypes' <- mapM (fill sub) dataTypes
  (consArgs', tree') <- fill''' sub consArgs tree
  return $ DT.Cons dd disc (zip dataTerms' dataTypes') consArgs' tree'
