module Move.Scene.WeakTerm.Reduce (reduce) where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseError, toApp)
import Move.Context.Env qualified as Env
import Move.Scene.WeakTerm.Subst qualified as Subst
import Rule.Attr.DataIntro qualified as AttrDI
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.Const
import Rule.DecisionTree qualified as DT
import Rule.Discriminant qualified as D
import Rule.Hint qualified as H
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.Module
import Rule.Source
import Rule.WeakPrim qualified as WP
import Rule.WeakPrimValue qualified as WPV
import Rule.WeakTerm qualified as WT

type InlineLimit =
  Int

type CurrentStep =
  Int

data Handle = Handle
  { substHandle :: Subst.Handle,
    currentStepRef :: IORef CurrentStep,
    inlineLimit :: InlineLimit,
    location :: H.Hint
  }

reduce :: WT.WeakTerm -> App WT.WeakTerm
reduce term@(m :< _) = do
  h <- new m
  toApp $ reduce' h term

new :: H.Hint -> App Handle
new location = do
  substHandle <- Subst.new
  source <- Env.getCurrentSource
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  currentStepRef <- liftIO $ newIORef 0
  return Handle {..}

reduce' :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
reduce' h term = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case term of
    m :< WT.Pi impArgs expArgs cod -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (reduce' h) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (reduce' h) ts
        return $ zip3 ms xs ts'
      cod' <- reduce' h cod
      return $ m :< WT.Pi impArgs' expArgs' cod'
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (reduce' h) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (reduce' h) ts
        return $ zip3 ms xs ts'
      e' <- reduce' h e
      case lamKind of
        LK.Fix (mx, x, t) -> do
          t' <- reduce' h t
          return (m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, t')}) impArgs' expArgs' e')
        _ ->
          return (m :< WT.PiIntro attr impArgs' expArgs' e')
    m :< WT.PiElim e es -> do
      e' <- reduce' h e
      es' <- mapM (reduce' h) es
      case e' of
        (_ :< WT.PiIntro AttrL.Attr {lamKind = LK.Normal _} impArgs expArgs body)
          | xts <- impArgs ++ expArgs,
            length xts == length es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst (substHandle h) sub body >>= reduce' h
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
      e' <- reduce' h e
      return $ m :< WT.PiElimExact e'
    m :< WT.Data attr name es -> do
      es' <- mapM (reduce' h) es
      return $ m :< WT.Data attr name es'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (reduce' h) dataArgs
      consArgs' <- mapM (reduce' h) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      detectPossibleInfiniteLoop h
      let (os, es, ts) = unzip3 oets
      es' <- mapM (reduce' h) es
      ts' <- mapM (reduce' h) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- reduceDecisionTree h decisionTree
          return $ m :< WT.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              Subst.subst (substHandle h) sub (WT.fromLetSeq letSeq e) >>= reduce' h
            DT.Unreachable ->
              return $ m :< WT.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< WT.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'')
                  | (newBaseCursorList, cont) <- findClause discriminant fallbackTree caseList -> do
                      let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                      let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                      cont' <- Subst.substDecisionTree (substHandle h) sub cont
                      reduce' h $ m :< WT.DataElim isNoetic (oets'' ++ newCursorList) cont'
                _ -> do
                  decisionTree' <- reduceDecisionTree h decisionTree
                  return $ m :< WT.DataElim isNoetic oets' decisionTree'
    m :< WT.Box t -> do
      t' <- reduce' h t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- reduce' h t
      return $ m :< WT.BoxNoema t'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (reduceBinder h) xts
      es' <- mapM (reduce' h) es
      e' <- reduce' h e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.Let opacity mxt@(_, x, _) e1 e2 -> do
      e1' <- reduce' h e1
      case opacity of
        WT.Clear -> do
          detectPossibleInfiniteLoop h
          let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
          Subst.subst (substHandle h) sub e2 >>= reduce' h
        _ -> do
          e2' <- reduce' h e2
          return $ m :< WT.Let opacity mxt e1' e2'
    m :< WT.Magic der -> do
      der' <- mapM (reduce' h) der
      return $ m :< WT.Magic der'
    m :< WT.Prim prim -> do
      prim' <- mapM (reduce' h) prim
      return $ m :< WT.Prim prim'
    _ ->
      return term

reduceBinder :: Handle -> BinderF WT.WeakTerm -> EIO (BinderF WT.WeakTerm)
reduceBinder h (m, x, t) = do
  t' <- reduce' h t
  return (m, x, t')

reduceDecisionTree ::
  Handle ->
  DT.DecisionTree WT.WeakTerm ->
  EIO (DT.DecisionTree WT.WeakTerm)
reduceDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (reduceBinder h) (reduce' h)) letSeq
      e' <- reduce' h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- reduce' h cursor
      clauseList' <- reduceCaseList h clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

reduceCaseList ::
  Handle ->
  DT.CaseList WT.WeakTerm ->
  EIO (DT.CaseList WT.WeakTerm)
reduceCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- reduceDecisionTree h fallbackTree
  clauseList' <- mapM (reduceCase h) clauseList
  return (fallbackTree', clauseList')

reduceCase ::
  Handle ->
  DT.Case WT.WeakTerm ->
  EIO (DT.Case WT.WeakTerm)
reduceCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- reduceDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (reduce' h) dataTerms
      dataTypes' <- mapM (reduce' h) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (reduce' h) ts
      cont' <- reduceDecisionTree h cont
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

detectPossibleInfiniteLoop :: Handle -> EIO ()
detectPossibleInfiniteLoop h = do
  let Handle {inlineLimit, currentStepRef, location} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)
