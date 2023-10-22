module Scene.WeakTerm.Subst (subst, substDecisionTree) where

import Context.App
import Context.Gensym
import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Data.Maybe (mapMaybe)
import Entity.Annotation qualified as AN
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.WeakTerm qualified as WT

subst :: WT.SubstWeakTerm -> WT.WeakTerm -> App WT.WeakTerm
subst sub term =
  case term of
    _ :< WT.Tau ->
      return term
    m :< WT.Var x
      | Just varOrTerm <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrTerm of
            Left x' ->
              return $ m :< WT.Var x'
            Right e ->
              return e
      | otherwise ->
          return term
    _ :< WT.VarGlobal {} ->
      return term
    m :< WT.Pi xts t -> do
      (xts', t') <- substBinder sub xts t
      return $ m :< WT.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- subst'' sub xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        _ -> do
          (xts', e') <- substBinder sub xts e
          return $ m :< WT.PiIntro kind xts' e'
    m :< WT.PiElim e es -> do
      e' <- subst sub e
      es' <- mapM (subst sub) es
      return $ m :< WT.PiElim e' es'
    m :< WT.Data name consNameList es -> do
      es' <- mapM (subst sub) es
      return $ m :< WT.Data name consNameList es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (subst sub) dataArgs
      consArgs' <- mapM (subst sub) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (subst sub) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- subst''' sub binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< WT.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< WT.Noema t -> do
      t' <- subst sub t
      return $ m :< WT.Noema t'
    m :< WT.Embody t e -> do
      t' <- subst sub t
      e' <- subst sub e
      return $ m :< WT.Embody t' e'
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
    _ :< WT.ResourceType {} ->
      return term
    m :< WT.Magic der -> do
      der' <- mapM (subst sub) der
      return $ m :< WT.Magic der'
    m :< WT.Annotation logLevel annot e -> do
      e' <- subst sub e
      case annot of
        AN.Type t -> do
          t' <- subst sub t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'

substBinder ::
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
substBinder sub binder e =
  case binder of
    [] -> do
      e' <- subst sub e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- substBinder sub' xts e
      return ((m, x', t') : xts', e')

subst'' ::
  WT.SubstWeakTerm ->
  BinderF WT.WeakTerm ->
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
subst'' sub (m, x, t) binder e = do
  t' <- subst sub t
  x' <- newIdentFromIdent x
  let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
  (xts', e') <- substBinder sub' binder e
  return ((m, x', t'), xts', e')

subst''' ::
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  DT.DecisionTree WT.WeakTerm ->
  App ([BinderF WT.WeakTerm], DT.DecisionTree WT.WeakTerm)
subst''' sub binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- substDecisionTree sub decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- subst sub t
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (Left x') sub
      (xts', e') <- subst''' sub' xts decisionTree
      return ((m, x', t') : xts', e')

substDecisionTree ::
  WT.SubstWeakTerm ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
substDecisionTree sub tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- subst sub e
      let xs' = mapMaybe (substLeafVar sub) xs
      return $ DT.Leaf xs' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursorVar' = substVar sub cursorVar
      cursor' <- subst sub cursor
      caseList' <- substCaseList sub caseList
      return $ DT.Switch (cursorVar', cursor') caseList'

substCaseList ::
  WT.SubstWeakTerm ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
substCaseList sub (fallbackClause, clauseList) = do
  fallbackClause' <- substDecisionTree sub fallbackClause
  clauseList' <- mapM (substCase sub) clauseList
  return (fallbackClause', clauseList')

substCase ::
  WT.SubstWeakTerm ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
substCase sub decisionCase = do
  let (dataTerms, dataTypes) = unzip $ DT.dataArgs decisionCase
  dataTerms' <- mapM (subst sub) dataTerms
  dataTypes' <- mapM (subst sub) dataTypes
  (consArgs', cont') <- subst''' sub (DT.consArgs decisionCase) (DT.cont decisionCase)
  return $
    decisionCase
      { DT.dataArgs = zip dataTerms' dataTypes',
        DT.consArgs = consArgs',
        DT.cont = cont'
      }

substLeafVar :: WT.SubstWeakTerm -> Ident -> Maybe Ident
substLeafVar sub leafVar =
  case IntMap.lookup (Ident.toInt leafVar) sub of
    Just (Left leafVar') ->
      return leafVar'
    Just (Right _) ->
      Nothing
    Nothing ->
      return leafVar

substVar :: WT.SubstWeakTerm -> Ident -> Ident
substVar sub x =
  case IntMap.lookup (Ident.toInt x) sub of
    Just (Left x') ->
      x'
    _ ->
      x
