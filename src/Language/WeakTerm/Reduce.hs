module Language.WeakTerm.Reduce
  ( Handle,
    new,
    reduce,
    reduceType,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint qualified as H

type InlineLimit =
  Int

type CurrentStep =
  Int

data Handle = Handle
  { substHandle :: Subst.Handle,
    inlineLimit :: InlineLimit,
    currentStepRef :: IORef CurrentStep,
    location :: H.Hint
  }

new :: Subst.Handle -> H.Hint -> InlineLimit -> IO Handle
new substHandle location inlineLimit = do
  currentStepRef <- liftIO $ newIORef 0
  return $ Handle {..}

reduce :: Handle -> WT.WeakTerm -> App WT.WeakTerm
reduce h e = do
  reduce' h e

reduce' :: Handle -> WT.WeakTerm -> App WT.WeakTerm
reduce' h term = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case term of
    m :< WT.PiIntro attr@(AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      impArgs' <- mapM (reduceBinder h) impArgs
      defaultArgs' <- mapM (bimapM (reduceBinder h) (reduce' h)) defaultArgs
      expArgs' <- mapM (reduceBinder h) expArgs
      e' <- reduce' h e
      case lamKind of
        LK.Fix opacity (mx, x, t) -> do
          t' <- reduceType h t
          return (m :< WT.PiIntro (attr {AttrL.lamKind = LK.Fix opacity (mx, x, t')}) impArgs' defaultArgs' expArgs' e')
        LK.Normal name codType -> do
          codType' <- reduceType h codType
          return (m :< WT.PiIntro (attr {AttrL.lamKind = LK.Normal name codType'}) impArgs' defaultArgs' expArgs' e')
    m :< WT.PiElim isNoetic e impArgs defaultArgs expArgs -> do
      e' <- reduce' h e
      impArgs' <- ImpArgs.traverseImpArgs (reduceType h) impArgs
      defaultArgs' <- DefaultArgs.traverseDefaultArgs (reduce' h) defaultArgs
      expArgs' <- mapM (reduce' h) expArgs
      if isNoetic
        then return $ m :< WT.PiElim isNoetic e' impArgs' defaultArgs' expArgs'
        else do
          case e' of
            (_ :< WT.PiIntro AttrL.Attr {lamKind = LK.Normal {}} impParams defaultParams expParams body)
              | ImpArgs.Unspecified <- impArgs',
                null impParams,
                xts <- map fst defaultParams ++ expParams,
                length xts == length expArgs' -> do
                  let xs = map (\(_, x, _) -> Ident.toInt x) xts
                  let subTerm = IntMap.fromList $ zip xs (map Right expArgs')
                  liftIO (Subst.subst (substHandle h) subTerm body) >>= reduce' h
            (_ :< WT.PiIntro AttrL.Attr {lamKind = LK.Normal {}} impParams defaultParams expParams body)
              | ImpArgs.FullySpecified impArgs'' <- impArgs',
                length impArgs'' == length impParams,
                xts <- map fst defaultParams ++ expParams,
                length xts == length expArgs' -> do
                  let impIds = map (\(_, x, _) -> Ident.toInt x) impParams
                  let subType = IntMap.fromList $ zip impIds (map Right impArgs'')
                  let xs = map (\(_, x, _) -> Ident.toInt x) xts
                  let subTerm = IntMap.fromList $ zip xs (map Right expArgs')
                  body' <- liftIO $ Subst.subst (substHandle h) subTerm body
                  body'' <- liftIO $ Subst.substTypeInTerm subType body'
                  reduce' h body''
            (_ :< WT.Prim (WPV.Op op))
              | ImpArgs.Unspecified <- impArgs',
                Just (op', cod) <- WPV.reflectFloatUnaryOp op,
                [Just value] <- map asPrimFloatValue expArgs' -> do
                  let floatType = m :< WT.PrimType cod
                  return $ m :< WT.Prim (WPV.Float floatType (op' value))
              | ImpArgs.Unspecified <- impArgs',
                Just (op', cod) <- WPV.reflectIntegerBinaryOp op,
                [Just value1, Just value2] <- map asPrimIntegerValue expArgs' -> do
                  let intType = m :< WT.PrimType cod
                  return $ m :< WT.Prim (WPV.Int intType (op' value1 value2))
              | ImpArgs.Unspecified <- impArgs',
                Just (op', cod) <- WPV.reflectFloatBinaryOp op,
                [Just value1, Just value2] <- map asPrimFloatValue expArgs' -> do
                  let floatType = m :< WT.PrimType cod
                  return $ m :< WT.Prim (WPV.Float floatType (op' value1 value2))
              | ImpArgs.Unspecified <- impArgs',
                Just (op', cod) <- WPV.reflectIntegerCmpOp op,
                [Just value1, Just value2] <- map asPrimIntegerValue expArgs' -> do
                  let intType = m :< WT.PrimType cod
                  return $ m :< WT.Prim (WPV.Int intType (op' value1 value2))
              | ImpArgs.Unspecified <- impArgs',
                Just (op', cod) <- WPV.reflectFloatCmpOp op,
                [Just value1, Just value2] <- map asPrimFloatValue expArgs' -> do
                  let intType = m :< WT.PrimType cod
                  return $ m :< WT.Prim (WPV.Int intType (op' value1 value2))
            _ ->
              return $ m :< WT.PiElim isNoetic e' impArgs' defaultArgs' expArgs'
    m :< WT.PiElimExact e -> do
      e' <- reduce' h e
      return $ m :< WT.PiElimExact e'
    m :< WT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (reduceType h) dataArgs
      consArgs' <- mapM (reduce' h) consArgs
      attr' <- reduceAttrDataIntro h attr
      return $ m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< WT.DataElim isNoetic oets decisionTree -> do
      detectPossibleInfiniteLoop h
      let (os, es, ts) = unzip3 oets
      es' <- mapM (reduce' h) es
      ts' <- mapM (reduceType h) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- reduceDecisionTree h decisionTree
          return $ m :< WT.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              liftIO (Subst.subst (substHandle h) sub (WT.fromLetSeq letSeq e)) >>= reduce' h
            DT.Unreachable ->
              return $ m :< WT.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< WT.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'')
                  | (newBaseCursorList, cont) <- findClause discriminant fallbackTree caseList -> do
                      let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                      let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                      cont' <- liftIO $ Subst.substDecisionTree (substHandle h) sub cont
                      reduce' h $ m :< WT.DataElim isNoetic (oets'' ++ newCursorList) cont'
                _ -> do
                  decisionTree' <- reduceDecisionTree h decisionTree
                  return $ m :< WT.DataElim isNoetic oets' decisionTree'
    m :< WT.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (reduceBinder h) xts
      es' <- mapM (reduce' h) es
      e' <- reduce' h e
      return $ m :< WT.BoxIntro (zip xts' es') e'
    m :< WT.BoxIntroLift e -> do
      e' <- reduce' h e
      return $ m :< WT.BoxIntroLift e'
    m :< WT.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (bimapM (reduceBinder h) (reduce' h)) castSeq
      (mxt', e1') <- bimapM (reduceBinder h) (reduce' h) (mxt, e1)
      uncastSeq' <- mapM (bimapM (reduceBinder h) (reduce' h)) uncastSeq
      e2' <- reduce' h e2
      return $ m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< WT.CodeIntro e -> do
      e' <- reduce' h e
      return $ m :< WT.CodeIntro e'
    m :< WT.CodeElim e -> do
      e' <- reduce' h e
      return $ m :< WT.CodeElim e'
    m :< WT.Actual e -> do
      e' <- reduce' h e
      return $ m :< WT.Actual e'
    m :< WT.Let opacity mxt@(_, x, _) e1 e2 -> do
      e1' <- reduce' h e1
      case opacity of
        WT.Clear -> do
          detectPossibleInfiniteLoop h
          let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
          liftIO (Subst.subst (substHandle h) sub e2) >>= reduce' h
        _ -> do
          e2' <- reduce' h e2
          return $ m :< WT.Let opacity mxt e1' e2'
    m :< WT.LetType (mx, x) e1 e2 -> do
      e1' <- reduce' h e1
      e2' <- reduce' h e2
      return $ m :< WT.LetType (mx, x) e1' e2'
    m :< WT.Magic (M.WeakMagic magic) -> do
      magic' <- reduceMagic h magic
      return $ m :< WT.Magic (M.WeakMagic magic')
    m :< WT.Prim prim -> do
      prim' <- mapM (reduceType h) prim
      return $ m :< WT.Prim prim'
    m :< WT.Annotation logLevel annot e -> do
      e' <- reduce' h e
      case annot of
        AN.Type t -> do
          t' <- reduceType h t
          return $ m :< WT.Annotation logLevel (AN.Type t') e'
    _ ->
      return term

reduceType :: Handle -> WT.WeakType -> App WT.WeakType
reduceType h ty = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case ty of
    _ :< WT.Tau ->
      return ty
    _ :< WT.TVar {} ->
      return ty
    _ :< WT.TVarGlobal {} ->
      return ty
    m :< WT.TyApp t args -> do
      t' <- reduceType h t
      args' <- mapM (reduceType h) args
      return $ m :< WT.TyApp t' args'
    m :< WT.Pi piKind impArgs defaultArgs expArgs cod -> do
      impArgs' <- mapM (reduceBinder h) impArgs
      defaultArgs' <- mapM (bimapM (reduceBinder h) (reduce' h)) defaultArgs
      expArgs' <- mapM (reduceBinder h) expArgs
      cod' <- reduceType h cod
      return $ m :< WT.Pi piKind impArgs' defaultArgs' expArgs' cod'
    m :< WT.Data attr name es -> do
      es' <- mapM (reduceType h) es
      attr' <- reduceAttrData h attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- reduceType h t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- reduceType h t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- reduceType h t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- reduceType h unitType
      discarder' <- reduce' h discarder
      copier' <- reduce' h copier
      typeTag' <- reduce' h typeTag
      return $ m :< WT.Resource dd resourceID unitType' discarder' copier' typeTag'
    m :< WT.TypeHole hole es -> do
      es' <- mapM (reduceType h) es
      return $ m :< WT.TypeHole hole es'

reduceBinder :: Handle -> BinderF WT.WeakType -> App (BinderF WT.WeakType)
reduceBinder h (m, x, t) = do
  t' <- reduceType h t
  return (m, x, t')

reduceDecisionTree ::
  Handle ->
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  App (DT.DecisionTree WT.WeakType WT.WeakTerm)
reduceDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (reduceBinder h) (reduce' h)) letSeq
      e' <- reduce' h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- reduceType h cursor
      clauseList' <- reduceCaseList h clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

reduceCaseList ::
  Handle ->
  DT.CaseList WT.WeakType WT.WeakTerm ->
  App (DT.CaseList WT.WeakType WT.WeakTerm)
reduceCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- reduceDecisionTree h fallbackTree
  clauseList' <- mapM (reduceCase h) clauseList
  return (fallbackTree', clauseList')

reduceCase ::
  Handle ->
  DT.Case WT.WeakType WT.WeakTerm ->
  App (DT.Case WT.WeakType WT.WeakTerm)
reduceCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- reduceDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (reduceType h) dataTerms
      dataTypes' <- mapM (reduceType h) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (reduceType h) ts
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
  DT.DecisionTree WT.WeakType WT.WeakTerm ->
  [DT.Case WT.WeakType WT.WeakTerm] ->
  (CaseInfo, DT.DecisionTree WT.WeakType WT.WeakTerm)
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
  [(Ident, WT.WeakType)]

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
    _ :< WT.Prim (WPV.Int _ value) ->
      return value
    _ ->
      Nothing

asPrimFloatValue :: WT.WeakTerm -> Maybe Double
asPrimFloatValue term = do
  case term of
    _ :< WT.Prim (WPV.Float _ value) ->
      return value
    _ ->
      Nothing

detectPossibleInfiniteLoop :: Handle -> App ()
detectPossibleInfiniteLoop h = do
  let Handle {location, currentStepRef, inlineLimit} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)

reduceAttrData :: Handle -> AttrD.Attr name (BinderF WT.WeakType) -> App (AttrD.Attr name (BinderF WT.WeakType))
reduceAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- reduceType h t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}

reduceAttrDataIntro :: Handle -> AttrDI.Attr name (BinderF WT.WeakType) -> App (AttrDI.Attr name (BinderF WT.WeakType))
reduceAttrDataIntro h attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- forM binders $ \(mx, x, t) -> do
      t' <- reduceType h t
      return (mx, x, t')
    return (cn, binders', cl)
  return $ attr {AttrDI.consNameList = consNameList'}

reduceMagic :: Handle -> M.Magic WT.WeakType WT.WeakType WT.WeakTerm -> App (M.Magic WT.WeakType WT.WeakType WT.WeakTerm)
reduceMagic h magic =
  case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- reduceLowMagic h lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag mid typeTagExpr typeExpr -> do
      typeTagExpr' <- reduceType h typeTagExpr
      typeExpr' <- reduceType h typeExpr
      return $ M.GetTypeTag mid typeTagExpr' typeExpr'
    M.GetConsSize typeExpr -> do
      typeExpr' <- reduceType h typeExpr
      return $ M.GetConsSize typeExpr'
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- reduceType h listExpr
      typeExpr' <- reduceType h typeExpr
      index' <- reduce' h index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.CompileError {} ->
      return magic

reduceLowMagic :: Handle -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> App (LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm)
reduceLowMagic h lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- reduceType h from
      to' <- reduceType h to
      value' <- reduce' h value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      t' <- reduceType h t
      unit' <- reduceType h unit
      value' <- reduce' h value
      pointer' <- reduce' h pointer
      return $ LM.Store t' unit' value' pointer'
    LM.Load t pointer -> do
      t' <- reduceType h t
      pointer' <- reduce' h pointer
      return $ LM.Load t' pointer'
    LM.Alloca t size -> do
      t' <- reduceType h t
      size' <- reduce' h size
      return $ LM.Alloca t' size'
    LM.External domList cod extFunName args varArgs -> do
      domList' <- mapM (reduceType h) domList
      cod' <- case cod of
        FCT.Cod t -> do
          t' <- reduceType h t
          return $ FCT.Cod t'
        FCT.Void ->
          return FCT.Void
      args' <- mapM (reduce' h) args
      varArgs' <- forM varArgs $ \(arg, t) -> do
        arg' <- reduce' h arg
        t' <- reduceType h t
        return (arg', t')
      return $ LM.External domList' cod' extFunName args' varArgs'
    LM.Global name t -> do
      t' <- reduceType h t
      return $ LM.Global name t'
    LM.OpaqueValue value -> do
      value' <- reduce' h value
      return $ LM.OpaqueValue value'
    LM.CallType func arg1 arg2 -> do
      func' <- reduce' h func
      arg1' <- reduce' h arg1
      arg2' <- reduce' h arg2
      return $ LM.CallType func' arg1' arg2'
    LM.TermType ty -> do
      ty' <- reduceType h ty
      return $ LM.TermType ty'
