{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Kernel.Clarify.Clarify
  ( Handle,
    new,
    MainHandle,
    newMain,
    clarify,
    clarifyEntryPoint,
  )
where

import App.App (App)
import App.Run (raiseCritical, raiseCritical')
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Handle.AuxEnv qualified as AuxEnv
import Kernel.Clarify.Internal.Handle.CompDef qualified as CompDef
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Sigma qualified as Sigma
import Kernel.Clarify.Internal.Utility (toRelevantApp)
import Kernel.Clarify.Internal.Utility qualified as Utility
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.OptimizableData qualified as OD
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BaseName qualified as BN
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataSize qualified as DS
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.Noema qualified as N
import Language.Common.Opacity (isOpaque)
import Language.Common.Opacity qualified as O
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PrimNumSize (dataSizeToIntSize)
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.Rune qualified as RU
import Language.Common.StmtKind qualified as SK
import Language.Common.VarKind qualified as VK
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar qualified as Gensym
import Language.Comp.EnumCase qualified as EC
import Language.Comp.Reduce qualified as Reduce
import Language.Comp.Subst qualified as CompSubst
import Language.Term.Chain (nubFreeVariables)
import Language.Term.Chain qualified as TM
import Language.Term.FromPrimNum
import Language.Term.PrimValue qualified as PV
import Language.Term.Stmt (Stmt, StmtF (..), isMacroStmt)
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    sigmaHandle :: Sigma.Handle,
    locatorHandle :: Locator.Handle,
    optDataHandle :: OptimizableData.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    compDefHandle :: CompDef.Handle,
    baseSize :: DS.DataSize,
    typeHandle :: Type.Handle
  }

new :: Global.Handle -> Local.Handle -> IO Handle
new (Global.Handle {..}) (Local.Handle {..}) = do
  let baseSize = Platform.getDataSize platformHandle
  auxEnvHandle <- AuxEnv.new
  defMap <- CompDef.get compDefHandle
  let substHandle = Subst.new gensymHandle
  let compSubstHandle = CompSubst.new gensymHandle
  let reduceHandle = Reduce.new compSubstHandle gensymHandle defMap
  let utilityHandle = Utility.new gensymHandle compSubstHandle auxEnvHandle baseSize
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let sigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle
  return $ Handle {..}

newAuxReduceHandle :: Handle -> IO Reduce.Handle
newAuxReduceHandle h = do
  let baseDefMap = Map.empty
  auxDefMap <- AuxEnv.get (auxEnvHandle h)
  let auxDefMap' = fromMaybe Map.empty $ mapM C.fromCompStmt auxDefMap
  let defMap' = Map.union baseDefMap auxDefMap'
  let compSubstHandle = CompSubst.new (gensymHandle h)
  return $ Reduce.new compSubstHandle (gensymHandle h) defMap'

clarify :: Handle -> [Stmt] -> App [C.CompStmt]
clarify h stmtList = do
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  baseAuxEnv <- liftIO $ getBaseAuxEnv (auxEnvHandle h) (sigmaHandle h)
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  let filteredStmtList = filter (not . isMacroStmt) stmtList
  stmtList' <- do
    stmtList' <- mapM (clarifyStmt h) filteredStmtList
    auxEnv <- liftIO $ AuxEnv.toCompStmtList <$> AuxEnv.get (auxEnvHandle h)
    return $ stmtList' ++ auxEnv
  forM_ (stmtList' ++ baseAuxEnv) $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        liftIO $ CompDef.insert (compDefHandle h) x (opacity, args, e)
      C.DefVoid x opacity args e -> do
        liftIO $ CompDef.insert (compDefHandle h) x (opacity, args, e)
      C.Foreign {} ->
        return ()
  auxReduceHandle <- liftIO $ newAuxReduceHandle h
  forM stmtList' $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce auxReduceHandle e
        -- liftIO $ putStrLn $ T.unpack "==================="
        -- liftIO $ putStrLn $ T.unpack $ DD.reify x
        -- liftIO $ putStrLn $ T.unpack $ T.pack $ show args
        -- liftIO $ putStrLn $ T.unpack $ T.pack $ show e'
        return $ C.Def x opacity args e'
      C.DefVoid x opacity args e -> do
        e' <- liftIO $ Reduce.reduce auxReduceHandle e
        return $ C.DefVoid x opacity args e'
      C.Foreign {} ->
        return stmt

data MainHandle = MainHandle
  { mainAuxEnvHandle :: AuxEnv.Handle,
    mainSigmaHandle :: Sigma.Handle,
    mainReduceHandle :: Reduce.Handle
  }

newMain :: Global.Handle -> IO MainHandle
newMain Global.Handle {..} = do
  mainAuxEnvHandle <- AuxEnv.new
  defMap <- CompDef.get compDefHandle
  let baseSize = Platform.getDataSize platformHandle
  let compSubstHandle = CompSubst.new gensymHandle
  let mainReduceHandle = Reduce.new compSubstHandle gensymHandle defMap
  let utilityHandle = Utility.new gensymHandle compSubstHandle mainAuxEnvHandle baseSize
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let mainSigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle
  return $ MainHandle {..}

clarifyEntryPoint :: MainHandle -> IO [C.CompStmt]
clarifyEntryPoint h = do
  baseAuxEnv <- getBaseAuxEnv (mainAuxEnvHandle h) (mainSigmaHandle h)
  forM baseAuxEnv $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- Reduce.reduce (mainReduceHandle h) e
        return $ C.Def x opacity args e'
      C.DefVoid x opacity args e -> do
        e' <- Reduce.reduce (mainReduceHandle h) e
        return $ C.DefVoid x opacity args e'
      C.Foreign {} ->
        return stmt

getBaseAuxEnv :: AuxEnv.Handle -> Sigma.Handle -> IO [C.CompStmt]
getBaseAuxEnv auxEnvHandle sigmaHandle = do
  Sigma.registerImmediateS4 sigmaHandle
  Sigma.registerClosureS4 sigmaHandle
  AuxEnv.toCompStmtList <$> AuxEnv.get auxEnvHandle

clarifyStmt :: Handle -> Stmt -> App C.CompStmt
clarifyStmt h stmt =
  case stmt of
    StmtDefine _ stmtKind _ f impArgs expArgs defaultArgs codType e -> do
      defaultValues <- registerDefaultFunctions h IntMap.empty f [] impArgs defaultArgs
      liftIO $ registerDefaultEnvType h f defaultValues
      let xts = impArgs ++ expArgs ++ map fst defaultArgs
      xts' <- dropFst <$> clarifyBinder h IntMap.empty xts
      envArg <- liftIO $ makeEnvArg h
      switchArg <- liftIO $ makeSwitchArg h
      let xts'' = xts' ++ [envArg, switchArg]
      let tenv = TM.insTypeEnv xts IntMap.empty
      if SK.isDestPassingStmtKind stmtKind
        then do
          e' <- clarifyTerm h tenv e
          codType' <- clarifyType h tenv codType
          destParam <- liftIO $ makeDestParam h
          e'' <- liftIO $ toDestPassing h destParam codType' e'
          e''' <- liftIO $ Linearize.linearize (linearizeHandle h) xts'' e'' >>= Reduce.reduce (reduceHandle h)
          return $ C.DefVoid f (SK.toLowOpacityTerm stmtKind) (destParam : map fst xts'') e'''
        else do
          e' <- clarifyStmtDefineBody h tenv xts'' e
          return $ C.Def f (SK.toLowOpacityTerm stmtKind) (map fst xts'') e'
    StmtDefineType _ stmtKind (SavedHint m) f impArgs expArgs defaultArgs _ body -> do
      defaultValues <- registerDefaultFunctions h IntMap.empty f [] impArgs defaultArgs
      liftIO $ registerDefaultEnvType h f defaultValues
      let xts = impArgs ++ expArgs ++ map fst defaultArgs
      xts' <- dropFst <$> clarifyBinder h IntMap.empty xts
      envArg <- liftIO $ makeEnvArg h
      switchArg <- liftIO $ makeSwitchArg h
      let xts'' = xts' ++ [envArg, switchArg]
      case stmtKind of
        SK.Data name dataArgs consInfoList -> do
          od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
          case od of
            Just OD.Enum -> do
              liftIO (Sigma.returnSigmaEnumS4 (sigmaHandle h) name O.Clear)
                >>= clarifyStmtDefineBody' h name xts''
            Just OD.Unary
              | [(_, _, _, [(_, _, _, t)], _)] <- consInfoList -> do
                  (dataArgs', t') <- clarifyTypeBinderBody h IntMap.empty dataArgs t
                  return $ C.Def f O.Clear (map fst $ dataArgs' ++ [envArg, switchArg]) t'
              | otherwise ->
                  raiseCritical m "Found a broken unary data"
            _ -> do
              let totalSlotCount = getDataSlotCount dataArgs consInfoList
              let dataInfo = map (\(_, consName, isConstLike, consArgs, discriminant) -> (consName, isConstLike, discriminant, dataArgs, consArgs)) consInfoList
              dataInfo' <- mapM (clarifyDataClause h totalSlotCount) dataInfo
              liftIO (Sigma.returnSigmaDataS4 (sigmaHandle h) name O.Opaque totalSlotCount dataInfo')
                >>= clarifyStmtDefineBody' h name xts''
        _ -> do
          let tenv = TM.insTypeEnv xts IntMap.empty
          body' <- clarifyStmtDefineTypeBody h tenv xts'' body
          return $ C.Def f (SK.toLowOpacityType stmtKind) (map fst xts'') body'
    StmtDefineResource (SavedHint m) dd resourceID _ discarder copier resourceSize -> do
      let liftedName = DD.makeResourceName dd resourceID
      switch <- liftIO $ Gensym.createVar (gensymHandle h) "switch"
      arg@(argVarName, _) <- liftIO $ Gensym.createVar (gensymHandle h) "arg"
      size <-
        clarifyTerm h IntMap.empty resourceSize
          >>= liftIO . Reduce.reduce (reduceHandle h)
      discard <-
        clarifyTerm h IntMap.empty (m :< TM.PiElim PEK.Normal discarder [] [m :< TM.Var argVarName] [])
          >>= liftIO . Reduce.reduce (reduceHandle h)
      copy <-
        clarifyTerm h IntMap.empty (m :< TM.PiElim PEK.Normal copier [] [m :< TM.Var argVarName] [])
          >>= liftIO . Reduce.reduce (reduceHandle h)
      let resourceSpec = Utility.ResourceSpec {switch, arg, discard, copy, size, defaultValues = []}
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear liftedName resourceSpec
      return $ C.Def dd O.Clear [] (C.UpIntro $ C.VarGlobal liftedName AN.argNumS4 (FCT.Cod BLT.Pointer))
    StmtVariadic {} -> do
      return $ C.Foreign [] -- nop
    StmtForeign foreignList ->
      return $ C.Foreign foreignList

makeEnvArg :: Handle -> IO (Ident, C.Comp)
makeEnvArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "env"
  return (x, Sigma.returnImmediateS4) -- top-level function's env is always null

makeSwitchArg :: Handle -> IO (Ident, C.Comp)
makeSwitchArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "sw"
  return (x, Sigma.returnImmediateS4)

makeDestParam :: Handle -> IO Ident
makeDestParam h = do
  Gensym.newIdentFromText (gensymHandle h) "dest"

getSizeComp :: Handle -> C.Comp -> IO C.Comp
getSizeComp h codType = do
  (typeName, typeVar) <- Gensym.createVar (gensymHandle h) "type"
  return $ C.UpElim True typeName codType (C.PiElimDownElim True typeVar [intTerm h (2 :: Int), C.null])

toDestPassing :: Handle -> Ident -> C.Comp -> C.Comp -> IO C.Comp
toDestPassing h dest codType e = do
  sizeComp <- getSizeComp h codType
  return $ C.WriteToDest (C.VarLocal dest) sizeComp e C.UpIntroVoid

defaultEnvTypeName :: DD.DefiniteDescription -> DD.DefiniteDescription
defaultEnvTypeName dd =
  DD.MakeDefiniteDescription {DD.reify = DD.reify dd <> "#default-env"}

hasDefaultArgs :: Handle -> DD.DefiniteDescription -> IO Bool
hasDefaultArgs h name = do
  mType <- Type.lookupMaybe' (typeHandle h) name
  return $ case mType of
    Just (_ :< WT.Pi _ _ _ defaultArgs _) ->
      not (null defaultArgs)
    Just (_ :< WT.BoxNoema (_ :< WT.Pi _ _ _ defaultArgs _)) ->
      not (null defaultArgs)
    _ ->
      False

envTypeForGlobal :: Handle -> DD.DefiniteDescription -> IO C.Value
envTypeForGlobal h name = do
  useImmediate <- not <$> hasDefaultArgs h name
  return $
    if useImmediate
      then Sigma.immediateS4
      else C.VarGlobal (defaultEnvTypeName name) AN.argNumS4 (FCT.Cod BLT.Pointer)

getGlobalRefInfo :: AttrVG.Attr -> (AN.ArgNum, FCT.ForeignCodType BLT.BaseLowType)
getGlobalRefInfo AttrVG.Attr {argNum, isDestPassing} =
  if isDestPassing
    then (AN.add argNum (AN.fromInt 3), FCT.Void)
    else (AN.add argNum (AN.fromInt 2), FCT.Cod BLT.Pointer)

defaultLabelName :: DD.DefiniteDescription -> Int -> DD.DefiniteDescription
defaultLabelName dd index =
  DD.MakeDefiniteDescription {DD.reify = DD.reify dd <> "#default" <> T.pack (show index)}

registerDefaultEnvType :: Handle -> DD.DefiniteDescription -> [C.Value] -> IO ()
registerDefaultEnvType h name defaultValues = do
  unless (null defaultValues) $ do
    let envTypeName = defaultEnvTypeName name
    isAlreadyRegistered <- AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) envTypeName
    unless isAlreadyRegistered $ do
      switch <- Gensym.createVar (gensymHandle h) "switch"
      arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
      let discard = C.UpIntro $ C.SigmaIntro []
      let copy = C.UpIntro argVar
      let resourceSpec = Utility.ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues}
      Utility.registerSwitcher (utilityHandle h) O.Clear envTypeName resourceSpec

registerDefaultFunctions ::
  Handle ->
  TM.TypeEnv ->
  DD.DefiniteDescription ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  App [C.Value]
registerDefaultFunctions h tenv name fvs impArgs defaultArgs =
  case defaultArgs of
    [] ->
      return []
    _ -> do
      let tenv' = TM.insTypeEnv (fvs ++ impArgs) tenv
      fvs' <- dropFst <$> clarifyBinder h tenv fvs
      impArgs' <- dropFst <$> clarifyBinder h tenv impArgs
      let argNum = AN.fromInt (length impArgs' + 2)
      forM (zip [0 ..] defaultArgs) $ \(i, ((_, _, _, codType), value)) -> do
        let labelName = defaultLabelName name i
        isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) labelName
        unless isAlreadyRegistered $ do
          codType' <- clarifyType h tenv' codType
          body <- clarifyTerm h tenv' value
          liftIO $ registerClosure h labelName O.Clear False codType' impArgs' fvs' body
        return $ C.VarGlobal labelName argNum (FCT.Cod BLT.Pointer)

clarifyBinderBody ::
  Handle ->
  TM.TypeEnv ->
  [BinderF TM.Type] ->
  TM.Term ->
  App ([(Ident, C.Comp)], C.Comp)
clarifyBinderBody h tenv xts e =
  case xts of
    [] -> do
      e' <- clarifyTerm h tenv e
      return ([], e')
    (m, k, x, t) : rest -> do
      t' <- clarifyType h tenv t
      (binder, e') <- clarifyBinderBody h (TM.insTypeEnv [(m, k, x, t)] tenv) rest e
      return ((x, t') : binder, e')

clarifyTypeBinderBody ::
  Handle ->
  TM.TypeEnv ->
  [BinderF TM.Type] ->
  TM.Type ->
  App ([(Ident, C.Comp)], C.Comp)
clarifyTypeBinderBody h tenv xts t =
  case xts of
    [] -> do
      t' <- clarifyType h tenv t
      return ([], t')
    (m, k, x, t1) : rest -> do
      t1' <- clarifyType h tenv t1
      (binder, t') <- clarifyTypeBinderBody h (TM.insTypeEnv [(m, k, x, t1)] tenv) rest t
      return ((x, t1') : binder, t')

clarifyStmtDefineBody ::
  Handle ->
  TM.TypeEnv ->
  [(Ident, C.Comp)] ->
  TM.Term ->
  App C.Comp
clarifyStmtDefineBody h tenv xts e = do
  clarifyTerm h tenv e
    >>= liftIO . Linearize.linearize (linearizeHandle h) xts
    >>= liftIO . Reduce.reduce (reduceHandle h)

clarifyStmtDefineTypeBody ::
  Handle ->
  TM.TypeEnv ->
  [(Ident, C.Comp)] ->
  TM.Type ->
  App C.Comp
clarifyStmtDefineTypeBody h tenv xts t = do
  clarifyType h tenv t
    >>= liftIO . Linearize.linearize (linearizeHandle h) xts
    >>= liftIO . Reduce.reduce (reduceHandle h)

clarifyStmtDefineBody' ::
  Handle ->
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  App C.CompStmt
clarifyStmtDefineBody' h name xts' dataType = do
  dataType' <- liftIO $ Linearize.linearize (linearizeHandle h) xts' dataType >>= Reduce.reduce (reduceHandle h)
  return $ C.Def name O.Clear (map fst xts') dataType'

clarifyTerm :: Handle -> TM.TypeEnv -> TM.Term -> App C.Comp
clarifyTerm h tenv term =
  case term of
    _ :< TM.Var x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.VarGlobal (AttrVG.Attr {..}) x -> do
      envType <- liftIO $ envTypeForGlobal h x
      let (globalArgNum, globalCodType) = getGlobalRefInfo AttrVG.Attr {..}
      return $
        C.UpIntro $
          C.SigmaIntro
            [ envType,
              C.SigmaIntro [],
              C.VarGlobal x globalArgNum globalCodType
            ]
    _ :< TM.PiIntro attr impArgs expArgs defaultArgs e -> do
      clarifyLambda h tenv attr (TM.chainOf tenv [term]) impArgs expArgs defaultArgs e
    _ :< TM.PiElim kind e impArgs expArgs defaultArgs -> do
      kind' <- PEK.traverseArg (clarifyType h tenv) kind
      impArgs' <- mapM (clarifyTypePlus h tenv) impArgs
      expArgs' <- mapM (clarifyPlus h tenv) expArgs
      defaultArgs' <- mapM (traverse (clarifyPlus h tenv)) defaultArgs
      let allArgs = impArgs' ++ expArgs' ++ catMaybes defaultArgs'
      case e of
        _ :< TM.Prim (PV.Op op) ->
          return $ callPrimOp op allArgs
        _ -> do
          e' <- clarifyTerm h tenv e
          liftIO $ callClosure h kind' e' impArgs' expArgs' defaultArgs'
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consName
      case od of
        Just OD.Enum ->
          return $ C.UpIntro $ C.Int (dataSizeToIntSize (baseSize h)) (D.reify discriminant)
        Just OD.Unary
          | [e] <- consArgs ->
              clarifyTerm h tenv e
          | otherwise ->
              raiseCritical m "Found a malformed unary data in Scene.Clarify.clarifyTerm"
        _ -> do
          (zs1, es1, xs1) <- unzip3 <$> mapM (clarifyTypePlus h tenv) dataArgs
          (zs2, es2, xs2) <- unzip3 <$> mapM (clarifyPlus h tenv) consArgs
          let totalSlotCount = 1 + length xs1 + foldr (max . (\(_, binders, _) -> length binders)) 0 consNameList
          return $
            Utility.bindLet (zip zs1 es1 ++ zip zs2 es2) $
              C.UpIntro $
                C.SigmaDataIntro totalSlotCount $
                  C.Int (dataSizeToIntSize (baseSize h)) (D.reify discriminant) : (xs1 ++ xs2)
    m :< TM.DataElim isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (\x -> (m, VK.Normal, x, m :< TM.Tau)) xs
      es' <- mapM (clarifyTerm h tenv) es
      (tree', _) <- clarifyDecisionTree h (TM.insTypeEnv mxts tenv) isNoetic IntMap.empty tree
      return $ Utility.irreducibleBindLet (zip xs es') tree'
    _ :< TM.BoxIntro letSeq e -> do
      embody h tenv letSeq e
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      clarifyTerm h tenv $
        TM.fromLetSeqOpaque castSeq $
          TM.fromLetSeq ((mxt, e1) : uncastSeq) e2
    _ :< TM.CodeIntro e -> do
      clarifyTerm h tenv e
    _ :< TM.CodeElim e -> do
      clarifyTerm h tenv e
    _ :< TM.TauIntro ty -> do
      clarifyType h tenv ty
    m :< TM.TauElim (mx, x) e1 e2 -> do
      clarifyTerm h tenv $ m :< TM.Let O.Clear (mx, VK.Normal, x, mx :< TM.Tau) e1 e2
    _ :< TM.Let opacity mxt@(_, _, x, _) e1 e2 -> do
      e2' <- clarifyTerm h (TM.insTypeEnv [mxt] tenv) e2
      mxts' <- dropFst <$> clarifyBinder h tenv [mxt]
      e2'' <- liftIO $ Linearize.linearize (linearizeHandle h) mxts' e2'
      e1' <- clarifyTerm h tenv e1
      return $ Utility.bindLetWithReducibility (not $ isOpaque opacity) [(x, e1')] e2''
    m :< TM.Prim primValue ->
      case primValue of
        PV.Int _ size l ->
          return $ C.UpIntro (C.Int size l)
        PV.Float _ size l ->
          return $ C.UpIntro (C.Float size l)
        PV.Op op -> do
          clarifyPrimOp h tenv op m
        PV.StaticText _ text ->
          return $ C.UpIntro $ C.VarStaticText text
        PV.Rune r -> do
          let t = fromPrimNum m (PT.Int PNS.IntSize32)
          clarifyTerm h tenv $ m :< TM.Prim (PV.Int t PNS.IntSize32 (RU.asInt r))
    _ :< TM.Magic der -> do
      clarifyMagic h tenv der

clarifyType :: Handle -> TM.TypeEnv -> TM.Type -> App C.Comp
clarifyType h tenv ty =
  case ty of
    _ :< TM.Tau -> do
      return Sigma.returnImmediateS4
    _ :< TM.TVar x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.TVarGlobal (AttrVG.Attr {..}) x -> do
      envType <- liftIO $ envTypeForGlobal h x
      let (globalArgNum, globalCodType) = getGlobalRefInfo AttrVG.Attr {..}
      return $
        C.UpIntro $
          C.SigmaIntro
            [ envType,
              C.SigmaIntro [],
              C.VarGlobal x globalArgNum globalCodType
            ]
    _ :< TM.TyApp t args -> do
      t' <- clarifyType h tenv t
      args' <- mapM (clarifyTypePlus h tenv) args
      liftIO $ callClosure h PEK.Normal t' args' [] []
    _ :< TM.Pi {} ->
      return Sigma.returnClosureS4
    _ :< TM.Data _ name dataArgs -> do
      let argNum = AN.fromInt $ length dataArgs + 2
      envType <- liftIO $ envTypeForGlobal h name
      let cls = C.UpIntro $ C.SigmaIntro [envType, C.SigmaIntro [], C.VarGlobal name argNum (FCT.Cod BLT.Pointer)]
      dataArgs' <- mapM (clarifyTypePlus h tenv) dataArgs
      liftIO $ callClosure h PEK.Normal cls dataArgs' [] []
    _ :< TM.Box t -> do
      clarifyType h tenv t
    _ :< TM.BoxNoema {} ->
      return Sigma.returnImmediateS4
    _ :< TM.Code t -> do
      clarifyType h tenv t
    _ :< TM.PrimType {} ->
      return Sigma.returnImmediateS4
    _ :< TM.Resource dd resourceID -> do
      return $ C.UpIntro $ C.VarGlobal (DD.makeResourceName dd resourceID) AN.argNumS4 (FCT.Cod BLT.Pointer)
    _ :< TM.Void ->
      return Sigma.returnImmediateS4

embody :: Handle -> TM.TypeEnv -> [(BinderF TM.Type, TM.Term)] -> TM.Term -> App C.Comp
embody h tenv xets cont =
  case xets of
    [] ->
      clarifyTerm h tenv cont
    (mxt@(_, _, x, t), e) : rest -> do
      t' <- clarifyType h tenv t
      (valueVarName, value, valueVar) <- clarifyPlus h tenv e
      relApp <- liftIO $ toRelevantApp (utilityHandle h) valueVar t'
      cont' <- embody h (TM.insTypeEnv [mxt] tenv) rest cont
      cont'' <- liftIO $ Linearize.linearize (linearizeHandle h) [(x, t')] cont'
      return $ Utility.bindLet [(valueVarName, value), (x, relApp)] cont''

type Size =
  Int

type DataArgsMap = IntMap.IntMap ([(Ident, TM.Type)], Size)

getDataSlotCount ::
  [BinderF TM.Type] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [BinderF TM.Type], D.Discriminant)] ->
  Int
getDataSlotCount dataArgs consInfoList =
  1 + length dataArgs + foldr max 0 (map (\(_, _, _, consArgs, _) -> length consArgs) consInfoList)

clarifyDataClause ::
  Handle ->
  Int ->
  (DD.DefiniteDescription, Bool, D.Discriminant, [BinderF TM.Type], [BinderF TM.Type]) ->
  App Sigma.DataConstructorInfo
clarifyDataClause h totalSlotCount (consNameVal, isConstLikeVal, discriminantVal, dataArgsVal, consArgsVal) = do
  args <- dropFst <$> clarifyBinder h IntMap.empty (dataArgsVal ++ consArgsVal)
  let (dataArgs', consArgs') = splitAt (length dataArgsVal) args
  return $
    Sigma.DataConstructorInfo
      { Sigma.consName = consNameVal,
        Sigma.isConstLike = isConstLikeVal,
        Sigma.discriminant = discriminantVal,
        Sigma.dataArgs = dataArgs',
        Sigma.consArgs = consArgs',
        Sigma.totalSlotCount = totalSlotCount
      }

clarifyDecisionTree ::
  Handle ->
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  DT.DecisionTree TM.Type TM.Term ->
  App (C.Comp, [BinderF TM.Type])
clarifyDecisionTree h tenv isNoetic dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList letSeq cont@(m :< _) -> do
      let chain = TM.chainOfDecisionTree tenv m tree
      cont' <- clarifyTerm h tenv $ TM.fromLetSeq letSeq cont
      if isNoetic
        then return (cont', chain)
        else do
          (cont'', dataChain) <- tidyCursorList h tenv dataArgsMap consumedCursorList cont'
          return (cont'', dataChain ++ chain)
    DT.Unreachable -> do
      return (C.Unreachable, [])
    DT.Switch (cursor, t@(m :< _)) (fallbackClause, clauseList) -> do
      (fallbackClause', fallbackChain) <- clarifyDecisionTree h tenv isNoetic dataArgsMap fallbackClause
      tmp <- mapM (clarifyCase h tenv isNoetic dataArgsMap cursor) clauseList
      let (enumCaseList, clauseList', clauseChainList) = unzip3 tmp
      let chain = nubFreeVariables $ fallbackChain ++ concat clauseChainList
      let aligner = alignFreeVariable h tenv chain
      clauseList'' <- mapM aligner clauseList'
      let newChain = (m, VK.Normal, cursor, m :< TM.Tau) : chain
      let idents = nubOrd $ map (\(_, _, x, _) -> x) newChain
      ck <- getClauseDataGroup h t
      case ck of
        Just OD.Enum -> do
          tree' <- liftIO $ Utility.getEnumElim (utilityHandle h) idents (C.VarLocal cursor) fallbackClause' (zip enumCaseList clauseList'')
          return (tree', newChain)
        Just OD.Unary -> do
          return (getFirstClause fallbackClause' clauseList'', newChain)
        _ -> do
          (disc, discVar) <- liftIO $ Gensym.createVar (gensymHandle h) "disc"
          enumElim <- liftIO $ Utility.getEnumElim (utilityHandle h) idents discVar fallbackClause' (zip enumCaseList clauseList'')
          return
            ( C.UpElim True disc (C.Primitive (C.Magic (LM.Load BLT.Pointer (C.VarLocal cursor)))) enumElim,
              newChain
            )

getFirstClause :: C.Comp -> [C.Comp] -> C.Comp
getFirstClause fallbackClause clauseList =
  case clauseList of
    [] ->
      fallbackClause
    clause : _ ->
      clause

getClauseDataGroup :: Handle -> TM.Type -> App (Maybe OD.OptimizableData)
getClauseDataGroup h term =
  case term of
    _ :< TM.Data _ dataName _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.TyApp t _ -> do
      getClauseDataGroup h t
    _ :< TM.TVarGlobal _ dataName -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.PrimType (PT.Int _) -> do
      return $ Just OD.Enum
    _ :< TM.PrimType PT.Rune -> do
      return $ Just OD.Enum
    _ ->
      raiseCritical' "Clarify.isEnumType"

tidyCursorList :: Handle -> TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> App (C.Comp, [BinderF TM.Type])
tidyCursorList h tenv dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return (cont, [])
    cursor : rest -> do
      case IntMap.lookup (Ident.toInt cursor) dataArgsMap of
        Nothing ->
          error "tidyCursor"
        Just (dataArgs, cursorSize) -> do
          let (dataArgVars, dataTypes) = unzip dataArgs
          dataTypes' <- mapM (clarifyType h tenv) dataTypes
          (cont', chain) <- tidyCursorList h tenv dataArgsMap rest cont
          tmp <- liftIO $ Linearize.linearize (linearizeHandle h) (zip dataArgVars dataTypes') $ do
            C.Free (C.VarLocal cursor) cursorSize cont'
          let newChain = zipWith (\x t@(m :< _) -> (m, VK.Normal, x, t)) dataArgVars dataTypes
          return (tmp, newChain ++ chain)

clarifyCase ::
  Handle ->
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  Ident ->
  DT.Case TM.Type TM.Term ->
  App (EC.EnumCase, C.Comp, [BinderF TM.Type])
clarifyCase h tenv isNoetic dataArgsMap cursor decisionCase = do
  case decisionCase of
    DT.LiteralCase _ l cont -> do
      (body', contChain) <- clarifyDecisionTree h tenv isNoetic dataArgsMap cont
      case l of
        L.Int i ->
          return (EC.Int i, body', contChain)
        L.Rune r ->
          return (EC.Int (RU.asInt r), body', contChain)
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (_, dataTypes) = unzip dataArgs
      dataArgVars <- liftIO $ mapM (const $ Gensym.newIdentFromText (gensymHandle h) "dataArg") dataTypes
      let cursorSize = 1 + length dataArgVars + length consArgs
      let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes, cursorSize) dataArgsMap
      let consArgs' = map (\(m, k, x, _) -> (m, k, x, m :< TM.Tau)) consArgs
      let prefixChain = TM.chainOfCaseWithoutCont tenv decisionCase
      (body', contChain) <- clarifyDecisionTree h (TM.insTypeEnv consArgs' tenv) isNoetic dataArgsMap' cont
      let consArgVars = map (\(_, _, x, _) -> x) consArgs
      let argVars = dataArgVars ++ consArgVars
      let contChain' = filter (\(_, _, x, _) -> x `notElem` argVars) contChain
      let chain = prefixChain ++ contChain'
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consDD
      case od of
        Just OD.Enum -> do
          return (EC.Int (D.reify disc), body', chain)
        Just OD.Unary
          | [(_, _, consArg, _)] <- consArgs ->
              return
                ( EC.Int 0,
                  C.UpElim True consArg (C.UpIntro (C.VarLocal cursor)) body',
                  chain
                )
          | otherwise ->
              raiseCritical' "Found a non-unary consArgs for unary ADT"
        _ -> do
          discriminantVar <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "discriminant"
          return
            ( EC.Int (D.reify disc),
              C.SigmaElim
                False
                (discriminantVar : dataArgVars ++ map (\(_, _, x, _) -> x) consArgs)
                (C.VarLocal cursor)
                body',
              chain
            )

alignFreeVariable :: Handle -> TM.TypeEnv -> [BinderF TM.Type] -> C.Comp -> App C.Comp
alignFreeVariable h tenv fvs e = do
  fvs' <- dropFst <$> clarifyBinder h tenv fvs
  liftIO $ Linearize.linearize (linearizeHandle h) fvs' e

clarifyMagic :: Handle -> TM.TypeEnv -> M.Magic BLT.BaseLowType TM.Type TM.Term -> App C.Comp
clarifyMagic h tenv der = do
  case der of
    M.LowMagic magic -> do
      case magic of
        LM.Cast from to value -> do
          (fromVarName, from', fromVar) <- clarifyTypePlus h tenv from
          (toVarName, to', toVar) <- clarifyTypePlus h tenv to
          (valueVarName, value', valueVar) <- clarifyPlus h tenv value
          return $
            Utility.bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
              C.Primitive (C.Magic (LM.Cast fromVar toVar valueVar))
        LM.Store lt unit value pointer -> do
          (unitVarName, unit', unitVar) <- clarifyTypePlus h tenv unit
          (valueVarName, value', valueVar) <- clarifyPlus h tenv value
          (pointerVarName, pointer', pointerVar) <- clarifyPlus h tenv pointer
          return $
            Utility.bindLet [(unitVarName, unit'), (valueVarName, value'), (pointerVarName, pointer')] $
              C.Primitive (C.Magic (LM.Store lt unitVar valueVar pointerVar))
        LM.Load lt pointer -> do
          (pointerVarName, pointer', pointerVar) <- clarifyPlus h tenv pointer
          return $
            Utility.bindLet [(pointerVarName, pointer')] $
              C.Primitive (C.Magic (LM.Load lt pointerVar))
        LM.Alloca lt size -> do
          (sizeVarName, size', sizeVar) <- clarifyPlus h tenv size
          return $
            Utility.bindLet [(sizeVarName, size')] $
              C.Primitive (C.Magic (LM.Alloca lt sizeVar))
        LM.External domList cod extFunName args varArgAndTypeList -> do
          (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus h tenv) args
          let (varArgs, varTypes) = unzip varArgAndTypeList
          (ys, varArgs', ysAsVarArgs) <- unzip3 <$> mapM (clarifyPlus h tenv) varArgs
          return $
            Utility.bindLet (zip xs args' ++ zip ys varArgs') $
              C.Primitive (C.Magic (LM.External domList cod extFunName xsAsVars (zip ysAsVarArgs varTypes)))
        LM.Global name lt ->
          return $ C.Primitive (C.Magic (LM.Global name lt))
        LM.OpaqueValue e ->
          clarifyTerm h tenv e
        LM.CallType func arg1 arg2 -> do
          (funcVarName, func', funcVar) <- clarifyPlus h tenv func
          (arg1VarName, arg1', arg1Var) <- clarifyPlus h tenv arg1
          (arg2VarName, arg2', arg2Var) <- clarifyPlus h tenv arg2
          return $
            Utility.bindLet [(funcVarName, func'), (arg1VarName, arg1'), (arg2VarName, arg2')] $
              C.Primitive (C.Magic (LM.CallType funcVar arg1Var arg2Var))
    M.InspectType {} ->
      error "InspectType should be evaluated during inline expansion"
    M.EqType {} ->
      error "EqType should be evaluated during inline expansion"
    M.ShowType _ _ ->
      error "ShowType should be evaluated during inline expansion"
    M.TextCons {} ->
      error "TextCons should be evaluated during inline expansion"
    M.TextUncons _ _ ->
      error "TextUncons should be evaluated during inline expansion"
    M.CompileError {} ->
      error "CompileError should be evaluated during inline expansion"

clarifyLambda ::
  Handle ->
  TM.TypeEnv ->
  AttrL.Attr TM.Type ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  TM.Term ->
  App C.Comp
clarifyLambda h tenv attrL@(AttrL.Attr {lamKind, identity}) fvs impArgs expArgs defaultArgs e@(m :< _) = do
  let mxts = impArgs ++ expArgs ++ map fst defaultArgs
  case lamKind of
    LK.Fix _ isDestPassing (_, _k, recFuncName, codType) -> do
      let liftedName = Locator.attachCurrentLocator (locatorHandle h) $ BN.muName recFuncName identity
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, _, x, _) -> mx :< TM.Var x) appArgs
      let argNum = AN.fromInt $ length appArgs'
      let attr = AttrVG.Attr {argNum, isConstLike = False, isDestPassing}
      let piElimKind = if isDestPassing then PEK.DestPass codType else PEK.Normal
      lamAttr <- do
        c <- liftIO $ Gensym.newCount (gensymHandle h)
        return $ AttrL.Attr {lamKind = LK.Normal (Just (Ident.toText recFuncName)) isDestPassing codType, identity = c}
      let lamApp =
            m
              :< TM.PiIntro
                lamAttr
                impArgs
                expArgs
                defaultArgs
                (m :< TM.PiElim piElimKind (m :< TM.VarGlobal attr liftedName) [] appArgs' [])
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        liftedBody <- liftIO $ Subst.subst (substHandle h) (IntMap.fromList [(Ident.toInt recFuncName, Subst.Term lamApp)]) e
        (liftedArgs, liftedBody') <- clarifyBinderBody h IntMap.empty appArgs liftedBody
        envArg <- liftIO $ makeEnvArg h
        switchArg <- liftIO $ makeSwitchArg h
        if isDestPassing
          then do
            codType' <- clarifyType h (TM.insTypeEnv appArgs IntMap.empty) codType
            destParam <- liftIO $ makeDestParam h
            liftedBody'' <- liftIO $ toDestPassing h destParam codType' liftedBody'
            liftedBody''' <- liftIO $ Linearize.linearize (linearizeHandle h) liftedArgs liftedBody'' >>= Reduce.reduce (reduceHandle h)
            liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (C.DefVoid liftedName O.Opaque (destParam : map fst liftedArgs ++ [fst envArg, fst switchArg]) liftedBody''')
          else do
            liftedBody'' <- liftIO $ Linearize.linearize (linearizeHandle h) liftedArgs liftedBody'
            liftedBody''' <- liftIO $ Reduce.reduce (reduceHandle h) liftedBody''
            liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (C.Def liftedName O.Opaque (map fst liftedArgs ++ [fst envArg, fst switchArg]) liftedBody''')
      liftIO $ registerDefaultEnvType h liftedName []
      clarifyTerm h tenv lamApp
    LK.Normal mName isDestPassing codType -> do
      let name = Locator.attachCurrentLocator (locatorHandle h) $ BN.lambdaName mName identity
      defaultValues <- registerDefaultFunctions h tenv name fvs impArgs defaultArgs
      e' <- clarifyTerm h (TM.insTypeEnv (catMaybes [AttrL.fromAttr attrL] ++ mxts) tenv) e
      returnClosure h tenv identity mName O.Clear isDestPassing codType fvs mxts defaultValues e'

clarifyPlus :: Handle -> TM.TypeEnv -> TM.Term -> App (Ident, C.Comp, C.Value)
clarifyPlus h tenv e = do
  e' <- clarifyTerm h tenv e
  (varName, var) <- liftIO $ Gensym.createVar (gensymHandle h) "var"
  return (varName, e', var)

clarifyTypePlus :: Handle -> TM.TypeEnv -> TM.Type -> App (Ident, C.Comp, C.Value)
clarifyTypePlus h tenv t = do
  t' <- clarifyType h tenv t
  (varName, var) <- liftIO $ Gensym.createVar (gensymHandle h) "var"
  return (varName, t', var)

clarifyBinder :: Handle -> TM.TypeEnv -> [BinderF TM.Type] -> App [(Hint, Ident, C.Comp)]
clarifyBinder h tenv binder =
  case binder of
    [] ->
      return []
    ((m, _, x, t) : xts) -> do
      t' <- clarifyType h tenv t
      xts' <- clarifyBinder h (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: Handle -> TM.TypeEnv -> PrimOp -> Hint -> App C.Comp
clarifyPrimOp h tenv op m = do
  let (domList, codType) = getTypeInfo op
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- liftIO $ mapAndUnzipM (const (Gensym.createVar (gensymHandle h) "prim")) domList
  let mxts = zipWith (\x t -> (m, VK.Normal, x, t)) xs argTypeList
  lamID <- liftIO $ Gensym.newCount (gensymHandle h)
  returnClosure h tenv lamID (Just "primOp") O.Clear False (fromPrimNum m codType) [] mxts [] $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  Handle ->
  TM.TypeEnv ->
  Int ->
  Maybe T.Text ->
  O.Opacity ->
  Bool ->
  TM.Type ->
  [BinderF TM.Type] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Type] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  [C.Value] -> -- default argument labels
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  App C.Comp
returnClosure h tenv lamID mName opacity isDestPassing codType fvs xts defaultValues e = do
  fvs'' <- dropFst <$> clarifyBinder h tenv fvs
  xts'' <- dropFst <$> clarifyBinder h tenv xts
  fvEnvSigma <- liftIO $ Sigma.closureEnvS4 (sigmaHandle h) mName (locatorHandle h) fvs'' defaultValues
  let fvEnv = C.SigmaIntro (map (\(x, _) -> C.VarLocal x) fvs'')
  let argNum = AN.fromInt $ length xts'' + if isDestPassing then 3 else 2
  let name = Locator.attachCurrentLocator (locatorHandle h) $ BN.lambdaName mName lamID
  isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) name
  unless isAlreadyRegistered $ do
    let codTypeTenv = TM.insTypeEnv (fvs ++ xts) tenv
    codType' <- clarifyType h codTypeTenv codType
    liftIO $ registerClosure h name opacity isDestPassing codType' xts'' fvs'' e
  let cod = if isDestPassing then FCT.Void else FCT.Cod BLT.Pointer
  return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name argNum cod]

registerClosure ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  Bool ->
  C.Comp ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  IO ()
registerClosure h name opacity isDestPassing codType xts fvs e = do
  (envVarName, envVar) <- Gensym.createVar (gensymHandle h) "env"
  (switchVarName, switchVar) <- Gensym.createVar (gensymHandle h) "switch"
  (slotNameList, slotVarList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "slot") fvs
  hole <- Gensym.newIdentFromText (gensymHandle h) "_"
  destParam <- makeDestParam h
  e' <- if isDestPassing then toDestPassing h destParam codType e else return e
  e'' <- liftIO $ Linearize.linearize (linearizeHandle h) (fvs ++ xts) e'
  normalPrefix <- lambdaPrefixNormal h fvs slotVarList envVar
  noeticPrefix <- lambdaPrefixNoetic h fvs slotVarList envVar
  let allocList =
        map (\slotName -> (slotName, C.Primitive (C.Magic (LM.Alloca BLT.Pointer (intTerm h (1 :: Int)))))) slotNameList
  enumElim <-
    Utility.getEnumElim (utilityHandle h) (envVarName : slotNameList) switchVar normalPrefix [(EC.Int 1, noeticPrefix)]
  let loadList =
        zipWith (\(x, _) slotVar -> (x, C.Primitive (C.Magic (LM.Load BLT.Pointer slotVar)))) fvs slotVarList
  let lambdaBody = Utility.bindLet (allocList ++ [(hole, enumElim)] ++ loadList) e''
  lambdaBody' <- liftIO $ Reduce.reduce (reduceHandle h) lambdaBody
  let args = map fst xts ++ [envVarName, switchVarName]
  if isDestPassing
    then AuxEnv.insert (auxEnvHandle h) name (C.DefVoid name opacity (destParam : args) lambdaBody')
    else AuxEnv.insert (auxEnvHandle h) name (C.Def name opacity args lambdaBody')

callClosure ::
  Handle ->
  PEK.PiElimKind C.Comp ->
  C.Comp ->
  [(Ident, C.Comp, C.Value)] ->
  [(Ident, C.Comp, C.Value)] ->
  [Maybe (Ident, C.Comp, C.Value)] ->
  IO C.Comp
callClosure h kind e impArgs expArgs defaultArgs = do
  let flag = if PEK.isNoetic kind then C.intValue1 else C.intValue0
  let (impNames, impComps, impVals) = unzip3 impArgs
  let (expNames, expComps, expVals) = unzip3 expArgs
  ((closureVarName, closureVar), envTypeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames h
  defaultTriples <- forM (zip [0 ..] defaultArgs) $ resolveDefaultTriple h envTypeVarName envVar impVals
  let (defNames, defComps, defVals) = unzip3 defaultTriples
  let args = impVals ++ expVals ++ defVals ++ [envVar, flag]
  callComp <- buildCall h kind lamVar args
  return $
    Utility.bindLet [(closureVarName, e)] $
      C.SigmaElim (not $ PEK.isNoetic kind) [envTypeVarName, envVarName, lamVarName] closureVar $
        Utility.bindLet (zip (impNames ++ expNames ++ defNames) (impComps ++ expComps ++ defComps)) callComp

resolveDefaultTriple ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  (Int, Maybe (Ident, C.Comp, C.Value)) ->
  IO (Ident, C.Comp, C.Value)
resolveDefaultTriple h envTypeVarName envVar impVals (i, mOverride) =
  case mOverride of
    Just triple ->
      return triple
    Nothing -> do
      (labelName, labelVar) <- Gensym.createVar (gensymHandle h) "label"
      let labelComp = C.PiElimDownElim False (C.VarLocal envTypeVarName) [intTerm h (i + 3), C.null]
      defaultComp <-
        Utility.bindLet [(labelName, labelComp)]
          <$> callDefaultLabel h envTypeVarName envVar impVals labelVar
      (defaultName, defaultVar) <- Gensym.createVar (gensymHandle h) "arg"
      return (defaultName, defaultComp, defaultVar)

buildCall ::
  Handle ->
  PEK.PiElimKind C.Comp ->
  C.Value ->
  [C.Value] ->
  IO C.Comp
buildCall h kind lamVar args =
  case kind of
    PEK.DestPass codType -> do
      sizeComp <- getSizeComp h codType
      return $ C.DestCall sizeComp lamVar args
    _ ->
      return $ C.PiElimDownElim False lamVar args

intTerm :: (Integral a) => Handle -> a -> C.Value
intTerm h i =
  C.Int (intSizeFrom h) (toInteger i)

intSizeFrom :: Handle -> PNS.IntSize
intSizeFrom h =
  dataSizeToIntSize (baseSize h)

callDefaultLabel ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
callDefaultLabel h envTypeVarName envVar impVals labelVar = do
  (envCopyName, envCopyVar) <- Gensym.createVar (gensymHandle h) "env"
  let envCopyComp = C.PiElimDownElim False (C.VarLocal envTypeVarName) [intTerm h (1 :: Int), envVar]
  return $
    Utility.bindLet [(envCopyName, envCopyComp)] $
      C.PiElimDownElim False labelVar (impVals ++ [envCopyVar, C.intValue0])

newClosureNames :: Handle -> IO ((Ident, C.Value), Ident, (Ident, C.Value), (Ident, C.Value))
newClosureNames h = do
  closureVarInfo <- Gensym.createVar (gensymHandle h) "closure"
  envTypeVarName <- Gensym.newIdentFromText (gensymHandle h) "exp"
  envVarInfo <- Gensym.createVar (gensymHandle h) "env"
  lamVarInfo <- Gensym.createVar (gensymHandle h) "thunk"
  return (closureVarInfo, envTypeVarName, envVarInfo, lamVarInfo)

callPrimOp :: PrimOp -> [(Ident, C.Comp, C.Value)] -> C.Comp
callPrimOp op zexes = do
  let (zs, es', xs) = unzip3 zexes
  Utility.bindLet (zip zs es') (C.Primitive (C.PrimOp op xs))

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs

lambdaPrefixNormal ::
  Handle ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
lambdaPrefixNormal h fvs slotVarList envVar = do
  body <- storeValuesInSlots h (map (C.VarLocal . fst) fvs) slotVarList
  return $ C.SigmaElim True (map fst fvs) envVar body

lambdaPrefixNoetic ::
  Handle ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
lambdaPrefixNoetic h fvs slotVarList envVar = do
  -- as == [APP-1, ..., APP-n]
  as <- forM fvs $ \(x, t) -> Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") fvs
  storeBody <- storeValuesInSlots h varList slotVarList
  body <- Linearize.linearize (linearizeHandle h) fvs $ Utility.bindLet (zip varNameList as) storeBody
  return $ C.SigmaElim False (map fst fvs) envVar body

storeValuesInSlots ::
  Handle ->
  [C.Value] ->
  [C.Value] ->
  IO C.Comp
storeValuesInSlots h valueList slotVarList = do
  ignoredVarList <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "_") valueList
  let storeBinderList =
        zip ignoredVarList $
          zipWith
            (\value slotVar -> C.Primitive (C.Magic (LM.Store BLT.Pointer C.null value slotVar)))
            valueList
            slotVarList
  return $ Utility.bindLet storeBinderList (C.UpIntro C.null)
