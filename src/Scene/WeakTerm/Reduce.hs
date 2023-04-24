module Scene.WeakTerm.Reduce (reduce) where

import Context.App
import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.DecisionTree qualified as DT
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Opacity qualified as O
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.FreeVars
import Scene.WeakTerm.Subst qualified as Subst

reduce :: WT.WeakTerm -> App WT.WeakTerm
reduce term =
  case term of
    m :< WT.Pi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      cod' <- reduce cod
      return $ m :< WT.Pi (zip3 ms xs ts') cod'
    m :< WT.PiIntro kind xts e
      | LK.Fix (_, x, _) <- kind,
        x `notElem` freeVars e ->
          reduce $ m :< WT.PiIntro (LK.Normal O.Transparent) xts e
      | otherwise -> do
          let (ms, xs, ts) = unzip3 xts
          ts' <- mapM reduce ts
          e' <- reduce e
          case kind of
            LK.Fix (mx, x, t) -> do
              t' <- reduce t
              return (m :< WT.PiIntro (LK.Fix (mx, x, t')) (zip3 ms xs ts') e')
            _ ->
              return (m :< WT.PiIntro kind (zip3 ms xs ts') e')
    m :< WT.PiElim e es -> do
      e' <- reduce e
      es' <- mapM reduce es
      case e' of
        (_ :< WT.PiIntro (LK.Normal O.Transparent) xts body)
          | length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst sub body >>= reduce
        _ ->
          return $ m :< WT.PiElim e' es'
    m :< WT.Data name consNameList es -> do
      es' <- mapM reduce es
      return $ m :< WT.Data name consNameList es'
    m :< WT.DataIntro dataName consName consNameList disc dataArgs consArgs -> do
      dataArgs' <- mapM reduce dataArgs
      consArgs' <- mapM reduce consArgs
      return $ m :< WT.DataIntro dataName consName consNameList disc dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM reduce es
      ts' <- mapM reduce ts
      decisionTree' <- reduceDecisionTree decisionTree
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts') decisionTree'
    m :< WT.Noema t -> do
      t' <- reduce t
      return $ m :< WT.Noema t'
    m :< WT.Embody t e -> do
      t' <- reduce t
      e' <- reduce e
      return $ m :< WT.Embody t' e'
    m :< WT.Cell t -> do
      t' <- reduce t
      return $ m :< WT.Cell t'
    m :< WT.CellIntro e -> do
      e' <- reduce e
      return $ m :< WT.CellIntro e'
    m :< WT.CellElim e -> do
      e' <- reduce e
      return $ m :< WT.CellElim e'
    m :< WT.Let opacity mxt@(_, x, _) e1 e2 -> do
      e1' <- reduce e1
      case opacity of
        WT.Opaque -> do
          e2' <- reduce e2
          return $ m :< WT.Let opacity mxt e1' e2'
        _ -> do
          let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
          Subst.subst sub e2
    m :< WT.Magic der -> do
      der' <- mapM reduce der
      return $ m :< WT.Magic der'
    m :< WT.Promise var t -> do
      t' <- reduce t
      return $ m :< WT.Promise var t'
    m :< WT.PromiseIntro pVar var (e, t) -> do
      e' <- reduce e
      t' <- reduce t
      return $ m :< WT.PromiseIntro pVar var (e', t')
    m :< WT.PromiseElim pVar var (e, t) -> do
      e' <- reduce e
      t' <- reduce t
      return $ m :< WT.PromiseElim pVar var (e', t')
    _ ->
      return term

reduceDecisionTree ::
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
reduceDecisionTree tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- reduce e
      return $ DT.Leaf xs e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- reduce cursor
      clauseList' <- reduceCaseList clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

reduceCaseList ::
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
reduceCaseList (fallbackTree, clauseList) = do
  fallbackTree' <- reduceDecisionTree fallbackTree
  clauseList' <- mapM reduceCase clauseList
  return (fallbackTree', clauseList')

reduceCase ::
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
reduceCase (DT.Cons mCons dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM reduce dataTerms
  dataTypes' <- mapM reduce dataTypes
  let (ms, xs, ts) = unzip3 consArgs
  ts' <- mapM reduce ts
  tree' <- reduceDecisionTree tree
  return $ DT.Cons mCons dd disc (zip dataTerms' dataTypes') (zip3 ms xs ts') tree'
