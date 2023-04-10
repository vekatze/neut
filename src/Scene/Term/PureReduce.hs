module Scene.Term.PureReduce (pureReduce) where

import Context.App
import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.DecisionTree qualified as DT
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Opacity qualified as O
import Entity.Term qualified as TM
import Scene.Term.Subst qualified as Subst

pureReduce :: TM.Term -> App TM.Term
pureReduce term =
  case term of
    (m :< TM.Pi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM pureReduce ts
      cod' <- pureReduce cod
      return (m :< TM.Pi (zip3 ms xs ts') cod')
    (m :< TM.PiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM pureReduce ts
      e' <- pureReduce e
      case kind of
        LK.Fix (mx, x, t) -> do
          t' <- pureReduce t
          return (m :< TM.PiIntro (LK.Fix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TM.PiIntro kind (zip3 ms xs ts') e')
    (m :< TM.PiElim e es) -> do
      e' <- pureReduce e
      es' <- mapM pureReduce es
      case e' of
        (_ :< TM.PiIntro (LK.Normal O.Transparent) xts (_ :< body))
          | length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst sub (m :< body) >>= pureReduce
        _ ->
          return (m :< TM.PiElim e' es')
    m :< TM.Data name es -> do
      es' <- mapM pureReduce es
      return $ m :< TM.Data name es'
    m :< TM.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM pureReduce dataArgs
      consArgs' <- mapM pureReduce consArgs
      return $ m :< TM.DataIntro dataName consName disc dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM pureReduce es
      ts' <- mapM pureReduce ts
      decisionTree' <- pureReduceDecisionTree decisionTree
      return $ m :< TM.DataElim isNoetic (zip3 os es' ts') decisionTree'
    (m :< TM.Magic der) -> do
      der' <- traverse pureReduce der
      return (m :< TM.Magic der')
    _ ->
      return term

pureReduceDecisionTree ::
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
pureReduceDecisionTree tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- pureReduce e
      return $ DT.Leaf xs e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- pureReduce cursor
      clauseList' <- pureReduceCaseList clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

pureReduceCaseList ::
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
pureReduceCaseList (fallbackTree, clauseList) = do
  fallbackTree' <- pureReduceDecisionTree fallbackTree
  clauseList' <- mapM pureReduceCase clauseList
  return (fallbackTree', clauseList')

pureReduceCase ::
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
pureReduceCase (DT.Cons m dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM pureReduce dataTerms
  dataTypes' <- mapM pureReduce dataTypes
  let (ms, xs, ts) = unzip3 consArgs
  ts' <- mapM pureReduce ts
  tree' <- pureReduceDecisionTree tree
  return $ DT.Cons m dd disc (zip dataTerms' dataTypes') (zip3 ms xs ts') tree'
