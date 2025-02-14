module Scene.WeakTerm.Reduce (reduce) where

import Context.App
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.Const
import Entity.DecisionTree qualified as DT
import Entity.Discriminant qualified as D
import Entity.Hint qualified as H
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Module
import Entity.Source
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.WeakTerm.Subst qualified as Subst

type InlineLimit =
  Int

type CurrentStep =
  Int

data Axis = Axis
  { currentStepRef :: IORef CurrentStep,
    inlineLimit :: InlineLimit,
    location :: H.Hint
  }

new :: H.Hint -> App Axis
new m = do
  source <- Env.getCurrentSource
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  currentStepRef <- liftIO $ newIORef 0
  return Axis {currentStepRef = currentStepRef, inlineLimit, location = m}

detectPossibleInfiniteLoop :: Axis -> App ()
detectPossibleInfiniteLoop axis = do
  let Axis {inlineLimit, currentStepRef, location} = axis
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    Throw.raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

incrementStep :: Axis -> App ()
incrementStep axis = do
  let Axis {currentStepRef} = axis
  liftIO $ modifyIORef' currentStepRef (+ 1)

reduce :: WT.WeakTerm -> App WT.WeakTerm
reduce term@(m :< _) = do
  ax <- new m
  reduce' ax term

reduce' :: Axis -> WT.WeakTerm -> App WT.WeakTerm
reduce' ax term = do
  detectPossibleInfiniteLoop ax
  incrementStep ax
  case term of
    m :< WT.Pi impArgs expArgs cod -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (reduce' ax) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (reduce' ax) ts
        return $ zip3 ms xs ts'
      cod' <- reduce' ax cod
      return $ m :< WT.Pi impArgs' expArgs' cod'
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (reduce' ax) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (reduce' ax) ts
        return $ zip3 ms xs ts'
      e' <- reduce' ax e
      case lamKind of
        LK.Fix (mx, x, t) -> do
          t' <- reduce' ax t
          return (m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, t')}) impArgs' expArgs' e')
        _ ->
          return (m :< WT.PiIntro attr impArgs' expArgs' e')
    m :< WT.PiElim e es -> do
      e' <- reduce' ax e
      es' <- mapM (reduce' ax) es
      case e' of
        (_ :< WT.PiIntro AttrL.Attr {lamKind = LK.Normal _} impArgs expArgs body)
          | xts <- impArgs ++ expArgs,
            length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst sub body >>= reduce' ax
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
    m :< WT.PiElimExact e -> do
      e' <- reduce' ax e
      return $ m :< WT.PiElimExact e'
    m :< WT.Data attr name es -> do
      es' <- mapM (reduce' ax) es
      return $ m :< WT.Data attr name es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (reduce' ax) dataArgs
      consArgs' <- mapM (reduce' ax) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      detectPossibleInfiniteLoop ax
      let (os, es, ts) = unzip3 oets
      es' <- mapM (reduce' ax) es
      ts' <- mapM (reduce' ax) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- reduceDecisionTree ax decisionTree
          return $ m :< WT.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              Subst.subst sub (WT.fromLetSeq letSeq e) >>= reduce' ax
            DT.Unreachable ->
              return $ m :< WT.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< WT.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'')
                  | (newBaseCursorList, cont) <- findClause discriminant fallbackTree caseList -> do
                      let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                      let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                      cont' <- Subst.substDecisionTree sub cont
                      reduce' ax $ m :< WT.DataElim isNoetic (oets'' ++ newCursorList) cont'
                _ -> do
                  decisionTree' <- reduceDecisionTree ax decisionTree
                  return $ m :< WT.DataElim isNoetic oets' decisionTree'
    m :< WT.Box t -> do
      t' <- reduce t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- reduce' ax t
      return $ m :< WT.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (reduceBinder ax) xts
      es' <- mapM (reduce' ax) es
      e' <- reduce e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.Let opacity mxt@(_, x, _) e1 e2 -> do
      e1' <- reduce' ax e1
      case opacity of
        WT.Clear -> do
          detectPossibleInfiniteLoop ax
          let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
          Subst.subst sub e2 >>= reduce' ax
        _ -> do
          e2' <- reduce' ax e2
          return $ m :< WT.Let opacity mxt e1' e2'
    m :< WT.Magic der -> do
      der' <- mapM (reduce' ax) der
      return $ m :< WT.Magic der'
    m :< WT.Prim prim -> do
      prim' <- mapM (reduce' ax) prim
      return $ m :< WT.Prim prim'
    _ ->
      return term

reduceBinder :: Axis -> BinderF WT.WeakTerm -> App (BinderF WT.WeakTerm)
reduceBinder ax (m, x, t) = do
  t' <- reduce' ax t
  return (m, x, t')

reduceDecisionTree ::
  Axis ->
  DT.DecisionTree WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakTerm)
reduceDecisionTree ax tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (reduceBinder ax) (reduce' ax)) letSeq
      e' <- reduce' ax e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- reduce' ax cursor
      clauseList' <- reduceCaseList ax clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

reduceCaseList ::
  Axis ->
  DT.CaseList WT.WeakTerm ->
  App (DT.CaseList WT.WeakTerm)
reduceCaseList ax (fallbackTree, clauseList) = do
  fallbackTree' <- reduceDecisionTree ax fallbackTree
  clauseList' <- mapM (reduceCase ax) clauseList
  return (fallbackTree', clauseList')

reduceCase ::
  Axis ->
  DT.Case WT.WeakTerm ->
  App (DT.Case WT.WeakTerm)
reduceCase axis decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- reduceDecisionTree axis cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (reduce' axis) dataTerms
      dataTypes' <- mapM (reduce' axis) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (reduce' axis) ts
      cont' <- reduceDecisionTree axis cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = zip3 ms xs ts',
              DT.cont = cont'
            }

findClause ::
  D.Discriminant ->
  DT.DecisionTree WT.WeakTerm ->
  [DT.Case WT.WeakTerm] ->
  (CaseInfo, DT.DecisionTree WT.WeakTerm)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], fallbackTree)
    clause : rest ->
      case DT.findCase consDisc clause of
        Just (consArgs, clauseTree) ->
          (consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

type CaseInfo =
  [(Ident, WT.WeakTerm)]

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
