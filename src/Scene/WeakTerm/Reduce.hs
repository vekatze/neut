module Scene.WeakTerm.Reduce (reduce) where

import Context.App
import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.DecisionTree qualified as DT
import Entity.Discriminant
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Opacity qualified as O
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.WeakTerm.Subst qualified as Subst

reduce :: WT.WeakTerm -> App WT.WeakTerm
reduce term =
  case term of
    m :< WT.Pi xts cod -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM reduce ts
      cod' <- reduce cod
      return $ m :< WT.Pi (zip3 ms xs ts') cod'
    m :< WT.PiIntro kind xts e -> do
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
        (_ :< WT.Prim (WP.Value (WPV.Op op)))
          | Just (op', cod) <- WPV.reflectFloatUnaryOp op,
            [Just value] <- map asPrimFloatValue es' -> do
              let floatType = m :< WT.Prim (WP.Type cod)
              return $ m :< WT.Prim (WP.Value (WPV.Float floatType (op' value)))
          | Just (op', cod) <- WPV.reflectIntegerBinaryOp op,
            [Just value1, Just value2] <- map asPrimIntegerValue es' -> do
              let intType = m :< WT.Prim (WP.Type cod)
              return $ m :< WT.Prim (WP.Value (WPV.Int intType (op' value1 value2)))
          | Just (op', cod) <- WPV.reflectFloatBinaryOp op,
            [Just value1, Just value2] <- map asPrimFloatValue es' -> do
              let floatType = m :< WT.Prim (WP.Type cod)
              return $ m :< WT.Prim (WP.Value (WPV.Float floatType (op' value1 value2)))
          | Just (op', cod) <- WPV.reflectIntegerCmpOp op,
            [Just value1, Just value2] <- map asPrimIntegerValue es' -> do
              let intType = m :< WT.Prim (WP.Type cod)
              return $ m :< WT.Prim (WP.Value (WPV.Int intType (op' value1 value2)))
          | Just (op', cod) <- WPV.reflectFloatCmpOp op,
            [Just value1, Just value2] <- map asPrimFloatValue es' -> do
              let intType = m :< WT.Prim (WP.Type cod)
              return $ m :< WT.Prim (WP.Value (WPV.Int intType (op' value1 value2)))
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
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- reduceDecisionTree decisionTree
          return $ m :< WT.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              Subst.subst sub e >>= reduce
            DT.Unreachable ->
              return $ m :< WT.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< WT.DataIntro _ _ _ disc _ consArgs), oets'')
                  | (newBaseCursorList, cont) <- findClause disc fallbackTree caseList -> do
                      let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                      let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                      cont' <- Subst.substDecisionTree sub cont
                      reduce $ m :< WT.DataElim isNoetic (oets'' ++ newCursorList) cont'
                _ -> do
                  decisionTree' <- reduceDecisionTree decisionTree
                  return $ m :< WT.DataElim isNoetic oets' decisionTree'
    m :< WT.Noema t -> do
      t' <- reduce t
      return $ m :< WT.Noema t'
    m :< WT.Embody t e -> do
      t' <- reduce t
      e' <- reduce e
      return $ m :< WT.Embody t' e'
    m :< WT.Let opacity mxt@(_, x, _) e1 e2 -> do
      e1' <- reduce e1
      case opacity of
        WT.Transparent -> do
          let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
          Subst.subst sub e2 >>= reduce
        _ -> do
          e2' <- reduce e2
          return $ m :< WT.Let opacity mxt e1' e2'
    m :< WT.Magic der -> do
      der' <- mapM reduce der
      return $ m :< WT.Magic der'
    m :< WT.Flow var t -> do
      t' <- reduce t
      return $ m :< WT.Flow var t'
    m :< WT.FlowIntro pVar var (e, t) -> do
      e' <- reduce e
      t' <- reduce t
      return $ m :< WT.FlowIntro pVar var (e', t')
    m :< WT.FlowElim pVar var (e, t) -> do
      e' <- reduce e
      t' <- reduce t
      return $ m :< WT.FlowElim pVar var (e', t')
    m :< WT.Prim prim -> do
      prim' <- mapM reduce prim
      return $ m :< WT.Prim prim'
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
reduceCase decisionCase = do
  case decisionCase of
    DT.Cons mCons dd disc dataArgs consArgs tree -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM reduce dataTerms
      dataTypes' <- mapM reduce dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM reduce ts
      tree' <- reduceDecisionTree tree
      return $ DT.Cons mCons dd disc (zip dataTerms' dataTypes') (zip3 ms xs ts') tree'

findClause ::
  Discriminant ->
  DT.DecisionTree WT.WeakTerm ->
  [DT.Case WT.WeakTerm] ->
  (CaseInfo, DT.DecisionTree WT.WeakTerm)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], fallbackTree)
    clause : rest ->
      case findCase consDisc clause of
        Just (consArgs, clauseTree) ->
          (consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

type CaseInfo =
  [(Ident, WT.WeakTerm)]

findCase :: Discriminant -> DT.Case WT.WeakTerm -> Maybe (CaseInfo, DT.DecisionTree WT.WeakTerm)
findCase consDisc decisionCase =
  case decisionCase of
    DT.Cons _ _ disc _ consArgs tree
      | consDisc == disc ->
          return (map (\(_, x, t) -> (x, t)) consArgs, tree)
    _ ->
      Nothing

lookupSplit :: Ident -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit cursor =
  lookupSplit' cursor []

lookupSplit' :: Ident -> [(Ident, b, c)] -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit' cursor acc oets =
  case oets of
    [] ->
      Nothing
    oet@(o, e, _) : rest ->
      if o == cursor
        then Just (e, reverse acc ++ rest)
        else lookupSplit' cursor (oet : acc) rest

asPrimIntegerValue :: WT.WeakTerm -> Maybe Integer
asPrimIntegerValue term = do
  case term of
    _ :< WT.Prim (WP.Value (WPV.Int _ value)) ->
      return value
    _ ->
      Nothing

asPrimFloatValue :: WT.WeakTerm -> Maybe Double
asPrimFloatValue term = do
  case term of
    _ :< WT.Prim (WP.Value (WPV.Float _ value)) ->
      return value
    _ ->
      Nothing
