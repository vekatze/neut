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
import Console.ReportMode qualified as Report
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Handle.AuxEnv qualified as AuxEnv
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Sigma qualified as Sigma
import Kernel.Clarify.Internal.Utility (toRelevantAppWith)
import Kernel.Clarify.Internal.Utility qualified as Utility
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.ImportedTypeDefCache qualified as ImportedTypeDefCache
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Resource qualified as Resource
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.OptimizableData qualified as OD
import Kernel.Common.Trace qualified as Trace
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CallConv qualified as CC
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataInfo qualified as DI
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.StorageWidth qualified as StorageWidth
import Language.Common.DataSize qualified as DS
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.Noema qualified as N
import Language.Common.Opacity (isOpaque)
import Language.Common.Opacity qualified as O
import Language.Common.SlotSize
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.Rune qualified as RU
import Language.Common.StmtKind qualified as SK
import Language.Common.VarKind qualified as VK
import Language.Common.CellLayout qualified as CL
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
import Language.Term.TraceID (noTrace)
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Logger.Hint

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    modulePathMap :: ModulePath.ModulePathMap,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    sigmaHandle :: Sigma.Handle,
    dataHandle :: Data.Handle,
    optDataHandle :: OptimizableData.Handle,
    resourceHandle :: Resource.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    typeHandle :: Type.Handle,
    typeDefHandle :: TypeDef.Handle,
    importedTypeDefCacheHandle :: ImportedTypeDefCache.Handle,
    dataSize :: DS.DataSize,
    closureNameSupplyRef :: IORef (Map.HashMap DD.DefiniteDescription Int),
    traceConfig :: Trace.Config,
    loggerHandle :: Logger.Handle
  }

data Context = Context
  { typeEnv :: TM.TypeEnv,
    currentFunction :: DD.DefiniteDescription
  }

newContext :: DD.DefiniteDescription -> Context
newContext currentFunction =
  Context {typeEnv = IntMap.empty, currentFunction}

extendContext :: [BinderF TM.Type] -> Context -> Context
extendContext xts context =
  context {typeEnv = TM.insTypeEnv xts (typeEnv context)}

setCurrentFunction :: DD.DefiniteDescription -> Context -> Context
setCurrentFunction currentFunction context =
  context {currentFunction}

new :: Gensym.Handle -> Global.Handle -> Trace.Config -> IO Handle
new gensymHandle (Global.Handle {..}) traceConfig = do
  modulePathMap <- ModulePath.get modulePathHandle
  auxEnvHandle <- AuxEnv.new
  let dataSize = Platform.getDataSize platformHandle
  let substHandle = Subst.new gensymHandle
  let compSubstHandle = CompSubst.new gensymHandle
  let reduceHandle = Reduce.new compSubstHandle gensymHandle mempty
  let utilityHandle = Utility.new gensymHandle compSubstHandle auxEnvHandle
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let sigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle dataSize
  closureNameSupplyRef <- newIORef Map.empty
  return $ Handle {..}

newReduceOnlyHandle :: Handle -> IO Handle
newReduceOnlyHandle h = do
  auxEnvHandle' <- AuxEnv.new
  let gensymHandle' = gensymHandle h
  let compSubstHandle = CompSubst.new gensymHandle'
  let utilityHandle' = Utility.new gensymHandle' compSubstHandle auxEnvHandle'
  let linearizeHandle' = Linearize.new gensymHandle' utilityHandle'
  let sigmaHandle' = Sigma.new gensymHandle' linearizeHandle' utilityHandle' (dataSize h)
  let reduceHandle' = Reduce.new compSubstHandle gensymHandle' mempty
  return $
    Handle
      { gensymHandle = gensymHandle',
        modulePathMap = modulePathMap h,
        linearizeHandle = linearizeHandle',
        utilityHandle = utilityHandle',
        auxEnvHandle = auxEnvHandle',
        sigmaHandle = sigmaHandle',
        dataHandle = dataHandle h,
        optDataHandle = optDataHandle h,
        resourceHandle = resourceHandle h,
        reduceHandle = reduceHandle',
        substHandle = substHandle h,
        typeHandle = typeHandle h,
        typeDefHandle = typeDefHandle h,
        importedTypeDefCacheHandle = importedTypeDefCacheHandle h,
        dataSize = dataSize h,
        closureNameSupplyRef = closureNameSupplyRef h,
        traceConfig = traceConfig h,
        loggerHandle = loggerHandle h
      }

newAuxReduceHandle :: Gensym.Handle -> C.DefMap -> IO Reduce.Handle
newAuxReduceHandle gensymHandle defMap = do
  let compSubstHandle = CompSubst.new gensymHandle
  return $ Reduce.new compSubstHandle gensymHandle defMap

clarify :: Handle -> [Stmt] -> App ([C.CompStmt], [C.CompStmt], C.DefMap)
clarify h stmtList = do
  let filteredStmtList = filter (not . isMacroStmt) stmtList
  (stmtList', auxEnv) <- do
    stmtList' <- mapM (clarifyStmt h) filteredStmtList
    auxEnv <- liftIO $ AuxEnv.toCompStmtList <$> AuxEnv.get (auxEnvHandle h)
    return (stmtList', auxEnv)
  liftIO $ mapM_ (reportTrace h Report.PreCompPhase "precomp") (stmtList' ++ auxEnv)
  baseAuxEnv <- liftIO $ makeBaseAuxEnv (sigmaHandle h)
  importedTypeDefList <- clarifyImportedTypeDefList h filteredStmtList
  let inlineableTypeDefList = mapMaybe inlineableTypeDef $ zip filteredStmtList stmtList'
  let defMap = C.toDefMap (importedTypeDefList ++ inlineableTypeDefList ++ auxEnv ++ baseAuxEnv)
  auxReduceHandle <- liftIO $ newAuxReduceHandle (gensymHandle h) defMap
  stmtList'' <- forM stmtList' $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce auxReduceHandle e
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt
      C.Expose {} ->
        return stmt
  auxEnv' <- forM auxEnv $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce auxReduceHandle e
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt
      C.Expose {} ->
        return stmt
  liftIO $ mapM_ (reportTrace h Report.CompPhase "comp") (stmtList'' ++ auxEnv')
  return (stmtList'', auxEnv', defMap)

reportTrace :: Handle -> Report.TracePhase -> T.Text -> C.CompStmt -> IO ()
reportTrace h phase stage stmt = do
  let pathMap = modulePathMap h
  case C.getCompStmtName stmt of
    Nothing ->
      return ()
    Just name -> do
      when (Trace.matches (traceConfig h) pathMap phase name) $ do
        Logger.trace (loggerHandle h) $ "[" <> stage <> "] " <> renderCompStmt stmt

renderCompStmt :: C.CompStmt -> T.Text
renderCompStmt stmt = do
  case stmt of
    C.Def name opacity args body ->
      renderCompDefinition (if isOpaque opacity then "define" else "inline") name args body
    C.Foreign {} ->
      "foreign declaration"
    C.Expose {} ->
      "export declaration"

renderCompDefinition :: T.Text -> DD.DefiniteDescription -> [Ident] -> C.Comp -> T.Text
renderCompDefinition keyword name args body =
  "\n"
    <> keyword
    <> " "
    <> DD.reify name
    <> "("
    <> T.intercalate ", " (map Ident.toText' args)
    <> ") {\n"
    <> C.renderComp 1 body
    <> "\n}"

inlineableTypeDef :: (Stmt, C.CompStmt) -> Maybe C.CompStmt
inlineableTypeDef (sourceStmt, compStmt) = do
  case sourceStmt of
    StmtDefineType {} -> do
      (opacity, _, _) <- C.fromCompStmt compStmt
      if opacity == O.Clear
        then return compStmt
        else Nothing
    _ ->
      Nothing

clarifyImportedTypeDefList :: Handle -> [Stmt] -> App [C.CompStmt]
clarifyImportedTypeDefList h stmtList = do
  typeDefMap <- liftIO $ TypeDef.get' (typeDefHandle h)
  let currentTypeNameList = mapMaybe stmtTypeDefName stmtList
  let typeDefList = filter (not . isCurrentTypeDef currentTypeNameList) $ Map.toList typeDefMap
  stmtList' <- mapM (uncurry $ clarifyImportedTypeDefCached h) typeDefList
  return $ deduplicateCompStmtList $ concat stmtList'

deduplicateCompStmtList :: [C.CompStmt] -> [C.CompStmt]
deduplicateCompStmtList stmtList =
  reverse $ deduplicateCompStmtList' S.empty [] stmtList

deduplicateCompStmtList' :: S.Set DD.DefiniteDescription -> [C.CompStmt] -> [C.CompStmt] -> [C.CompStmt]
deduplicateCompStmtList' nameSet acc stmtList =
  case stmtList of
    [] ->
      acc
    stmt : rest -> do
      case C.getCompStmtName stmt of
        Nothing ->
          deduplicateCompStmtList' nameSet (stmt : acc) rest
        Just name ->
          if S.member name nameSet
            then deduplicateCompStmtList' nameSet acc rest
            else deduplicateCompStmtList' (S.insert name nameSet) (stmt : acc) rest

clarifyImportedTypeDefCached :: Handle -> DD.DefiniteDescription -> TypeDef.TypeDefInfo -> App [C.CompStmt]
clarifyImportedTypeDefCached h name typeDefInfo = do
  stmtList <-
    ImportedTypeDefCache.getOrInsert (importedTypeDefCacheHandle h) name $ do
      reduceOnlyHandle <- liftIO $ newReduceOnlyHandle h
      stmt <- clarifyImportedTypeDef reduceOnlyHandle name typeDefInfo
      auxEnv <- liftIO $ AuxEnv.toCompStmtList <$> AuxEnv.get (auxEnvHandle reduceOnlyHandle)
      return $ stmt : auxEnv
  let compSubstHandle = CompSubst.new (gensymHandle h)
  liftIO $ mapM (CompSubst.refreshStmt compSubstHandle) stmtList

stmtTypeDefName :: Stmt -> Maybe DD.DefiniteDescription
stmtTypeDefName stmt =
  case stmt of
    StmtDefineType _ _ _ name _ _ _ _ _ ->
      Just name
    _ ->
      Nothing

isCurrentTypeDef :: [DD.DefiniteDescription] -> (DD.DefiniteDescription, TypeDef.TypeDefInfo) -> Bool
isCurrentTypeDef currentTypeNameList (name, _) =
  name `elem` currentTypeNameList

clarifyImportedTypeDef :: Handle -> DD.DefiniteDescription -> TypeDef.TypeDefInfo -> App C.CompStmt
clarifyImportedTypeDef h name typeDefInfo = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) name
  case dataInfoOrNone of
    Just dataInfo0 -> do
      dataInfo <- liftIO $ refreshDataInfo h dataInfo0
      clarifyDataTypeDef h name (DI.dataArgs dataInfo) (DI.consInfoList dataInfo)
    Nothing -> do
      typeDefInfo' <- liftIO $ refreshTypeDefInfo h typeDefInfo
      clarifyAliasTypeDef h name typeDefInfo'

refreshBinderList :: Handle -> Subst.Subst -> [BinderF TM.Type] -> IO ([BinderF TM.Type], Subst.Subst)
refreshBinderList h sub xts =
  case xts of
    [] ->
      return ([], sub)
    (m, k, x, t) : rest -> do
      t' <- Subst.substType (substHandle h) sub t
      x' <- Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (Subst.Var x') sub
      (rest', sub'') <- refreshBinderList h sub' rest
      return ((m, k, x', t') : rest', sub'')

refreshTypeDefInfo :: Handle -> TypeDef.TypeDefInfo -> IO TypeDef.TypeDefInfo
refreshTypeDefInfo h typeDefInfo = do
  (binders', sub) <- refreshBinderList h IntMap.empty (TypeDef.typeDefBinders typeDefInfo)
  body' <- Subst.substType (substHandle h) sub (TypeDef.typeDefBody typeDefInfo)
  return typeDefInfo {TypeDef.typeDefBinders = binders', TypeDef.typeDefBody = body'}

refreshDataInfo :: Handle -> DI.DataInfo (BinderF TM.Type) -> IO (DI.DataInfo (BinderF TM.Type))
refreshDataInfo h dataInfo = do
  (dataArgs', sub) <- refreshBinderList h IntMap.empty (DI.dataArgs dataInfo)
  consInfoList' <- mapM (refreshConsInfo h sub) (DI.consInfoList dataInfo)
  return dataInfo {DI.dataArgs = dataArgs', DI.consInfoList = consInfoList'}

refreshConsInfo :: Handle -> Subst.Subst -> DI.ConsInfo (BinderF TM.Type) -> IO (DI.ConsInfo (BinderF TM.Type))
refreshConsInfo h sub consInfo = do
  (consArgs', _) <- refreshBinderList h sub (DI.consArgs consInfo)
  return consInfo {DI.consArgs = consArgs'}

clarifyAliasTypeDef :: Handle -> DD.DefiniteDescription -> TypeDef.TypeDefInfo -> App C.CompStmt
clarifyAliasTypeDef h name typeDefInfo = do
  let binders = TypeDef.typeDefBinders typeDefInfo
  let body = TypeDef.typeDefBody typeDefInfo
  let context = newContext name
  binders' <- dropFst <$> clarifyBinder h context binders
  envArg <- liftIO $ makeEnvArg h
  switchArg <- liftIO $ makeSwitchArg h
  let binders'' = binders' ++ [envArg, switchArg]
  body' <- clarifyStmtDefineTypeBody h (extendContext binders context) binders'' body
  return $ C.Def name O.Clear (map fst binders'') body'

data MainHandle = MainHandle
  { mainAuxEnvHandle :: AuxEnv.Handle,
    mainSigmaHandle :: Sigma.Handle,
    mainGensymHandle :: Gensym.Handle
  }

newMain :: Gensym.Handle -> DS.DataSize -> IO MainHandle
newMain gensymHandle dataSize = do
  mainAuxEnvHandle <- AuxEnv.new
  let compSubstHandle = CompSubst.new gensymHandle
  let utilityHandle = Utility.new gensymHandle compSubstHandle mainAuxEnvHandle
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let mainSigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle dataSize
  let mainGensymHandle = gensymHandle
  return $ MainHandle {..}

clarifyEntryPoint :: MainHandle -> IO ([C.CompStmt], C.DefMap)
clarifyEntryPoint h = do
  baseAuxEnv <- makeBaseAuxEnv (mainSigmaHandle h)
  let defMap = C.toDefMap baseAuxEnv
  let compSubstHandle = CompSubst.new (mainGensymHandle h)
  let reduceHandle = Reduce.new compSubstHandle (mainGensymHandle h) defMap
  stmtList <- forM baseAuxEnv $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- Reduce.reduce reduceHandle e
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt
      C.Expose {} ->
        return stmt
  return (stmtList, defMap)

makeBaseAuxEnv :: Sigma.Handle -> IO [C.CompStmt]
makeBaseAuxEnv sigmaHandle = do
  immS4 <- Sigma.makeImmediateS4 sigmaHandle
  clsS4 <- Sigma.makeClosureS4 sigmaHandle
  return [immS4, clsS4]

clarifyStmt :: Handle -> Stmt -> App C.CompStmt
clarifyStmt h stmt =
  case stmt of
    StmtDefine _ stmtKind _ f impArgs expArgs defaultArgs codType e -> do
      let context = newContext f
      defaultValues <- registerDefaultFunctions h context f [] impArgs expArgs defaultArgs
      liftIO $ registerDefaultEnvType h f defaultValues
      let xts = impArgs ++ expArgs ++ map fst defaultArgs
      xts' <- dropFst <$> clarifyBinder h context xts
      let slots = sourceSlotsOf xts
      envArg <- liftIO $ makeEnvArg h
      switchArg <- liftIO $ makeSwitchArg h
      let xts'' = xts' ++ [envArg, switchArg]
      let context' = extendContext xts context
      let opacity = SK.toLowOpacityTerm stmtKind
      e' <- clarifyTerm h context' e
      (destParam, e'') <-
        if SK.isDestPassingStmtKind stmtKind
          then do
            codType' <- clarifyType h context' codType
            liftIO $ toDestPassing h codType' e'
          else
            return (Nothing, e')
      liftIO $ defineWithSourceEntries h f opacity destParam slots xts' xts'' [fst envArg, fst switchArg] id e''
    StmtDefineType _ stmtKind (SavedHint _) f impArgs expArgs defaultArgs _ body -> do
      let context = newContext f
      defaultValues <- registerDefaultFunctions h context f [] impArgs expArgs defaultArgs
      liftIO $ registerDefaultEnvType h f defaultValues
      let xts = impArgs ++ expArgs ++ map fst defaultArgs
      case stmtKind of
        SK.Data name dataArgs consInfoList _ -> do
          let consInfoList' = map snd consInfoList
          clarifyDataTypeDef h name dataArgs consInfoList'
        _ -> do
          xts' <- dropFst <$> clarifyBinder h context xts
          envArg <- liftIO $ makeEnvArg h
          switchArg <- liftIO $ makeSwitchArg h
          let xts'' = xts' ++ [envArg, switchArg]
          let context' = extendContext xts context
          body' <- clarifyStmtDefineTypeBody h context' xts'' body
          return $ C.Def f (SK.toLowOpacityType stmtKind) (map fst xts'') body'
    StmtDefineResource (SavedHint m) dd resourceID _ discarder copier resourceSize -> do
      let liftedName = DD.makeResourceName dd resourceID
      let context = newContext liftedName
      switch <- liftIO $ Gensym.createVar (gensymHandle h) "switch"
      arg@(argVarName, _) <- liftIO $ Gensym.createVar (gensymHandle h) "arg"
      extra@(extraVarName, _) <- liftIO $ Gensym.createVar (gensymHandle h) "extra"
      size <- clarifyResourceSize resourceSize
      discard <- clarifyTerm h context (m :< TM.PiElim noTrace CC.normal discarder [] [m :< TM.Var argVarName, m :< TM.Var extraVarName] [])
      copy <- clarifyTerm h context (m :< TM.PiElim noTrace CC.normal copier [] [m :< TM.Var argVarName, m :< TM.Var extraVarName] [])
      let resourceSpec = Utility.ResourceSpec {switch, arg, extra, discard, copy, size, defaultValues = []}
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear liftedName resourceSpec
      return $ C.Def dd O.Clear [] (C.UpIntro $ C.VarGlobal liftedName AN.argNumS4 (FCT.Cod BLT.Pointer))
    StmtTrope {} -> do
      return $ C.Foreign [] -- nop
    StmtVariadic {} -> do
      return $ C.Foreign [] -- nop
    StmtForeign foreignList ->
      return $ C.Foreign foreignList
    StmtExpose exportList ->
      return $ C.Expose $ map (\(SavedHint m, dd, extName) -> (m, dd, extName)) exportList
    StmtNamespace {} -> do
      return $ C.Foreign [] -- nop

sourceSlotsOf :: [BinderF TM.Type] -> [Bool]
sourceSlotsOf =
  map $ \(_, k, _, _) -> VK.isSource k

defineWithSourceEntries ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  Maybe Ident ->
  [Bool] ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  [Ident] ->
  (C.Comp -> C.Comp) ->
  C.Comp ->
  IO C.CompStmt
defineWithSourceEntries h f opacity destParam slots binders linearBinders suffixParams wrapEntryBody body = do
  (entryParams, claimBindings) <- newEntryLayout h slots binders
  body' <- Linearize.linearizeUser (linearizeHandle h) linearBinders body
  let entryParamList = maybeToList destParam ++ entryParams ++ suffixParams
  return $ C.Def f opacity entryParamList (wrapEntryBody (Utility.bindLet claimBindings body'))

newEntryLayout ::
  Handle ->
  [Bool] ->
  [(Ident, C.Comp)] ->
  IO ([Ident], [(Ident, C.Comp)])
newEntryLayout h slots binders = do
  entries <- forM (zip slots binders) $ \(isSourceSlot, (x, t')) ->
    if isSourceSlot
      then do
        sourceName <- Gensym.newIdentFromText (gensymHandle h) (Ident.toText x <> "-source")
        sizeComp <- getSizeComp h t'
        claim <- claimSource h sizeComp sourceName
        return (sourceName, [(x, claim)])
      else
        return (x, [])
  let (params, claimBindings) = unzip entries
  return (params, concat claimBindings)

claimSource :: Handle -> C.Comp -> Ident -> IO C.Comp
claimSource h sizeComp sourceName = do
  (sizeName, sizeVar) <- Gensym.createVar (gensymHandle h) "size"
  (cellName, cellVar) <- Gensym.createVar (gensymHandle h) "cell"
  ignored <- Gensym.newIdentFromText (gensymHandle h) "_"
  return $
    C.UpElim True sizeName sizeComp $
      C.UpElim True cellName (C.Primitive $ C.Alloc sizeVar) $
        C.UpElim True ignored (C.Primitive $ C.Memcpy cellVar (C.VarLocal sourceName) sizeVar) $
          C.UpIntro cellVar

makeEnvArg :: Handle -> IO (Ident, C.Comp)
makeEnvArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "env"
  return (x, Sigma.returnImmediateS4) -- top-level function's env is always null

makeSwitchArg :: Handle -> IO (Ident, C.Comp)
makeSwitchArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "sw"
  return (x, Sigma.returnImmediateS4)

getSizeComp :: Handle -> C.Comp -> IO C.Comp
getSizeComp h codType = do
  (typeName, typeVar) <- Gensym.createVar (gensymHandle h) "type"
  return $ C.UpElim True typeName codType (C.PiElimDownElim True typeVar [intTerm (2 :: Int), C.null, C.null])

toDestPassing :: Handle -> C.Comp -> C.Comp -> IO (Maybe Ident, C.Comp)
toDestPassing h codType e = do
  dest <- Gensym.newIdentFromText (gensymHandle h) "dest"
  sizeComp <- getSizeComp h codType
  return (Just dest, C.OutputProvide (C.VarLocal dest) sizeComp e)

defaultEnvTypeName :: DD.DefiniteDescription -> DD.DefiniteDescription
defaultEnvTypeName dd =
  DD.appendText dd "#default-env"

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
getGlobalRefInfo AttrVG.Attr {argNum, isDestPassing} = do
  let extraArgNum = if isDestPassing then 3 else 2
  (AN.add argNum (AN.fromInt extraArgNum), FCT.Cod BLT.Pointer)

defaultLabelName :: DD.DefiniteDescription -> Int -> DD.DefiniteDescription
defaultLabelName dd index =
  DD.appendText dd $ "#default" <> T.pack (show index)

registerDefaultEnvType :: Handle -> DD.DefiniteDescription -> [C.Value] -> IO ()
registerDefaultEnvType h name defaultValues = do
  unless (null defaultValues) $ do
    let envTypeName = defaultEnvTypeName name
    isAlreadyRegistered <- AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) envTypeName
    unless isAlreadyRegistered $ do
      switch <- Gensym.createVar (gensymHandle h) "switch"
      arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
      extra <- Gensym.createVar (gensymHandle h) "extra"
      let discard = C.UpIntro C.null
      let copy = C.UpIntro argVar
      let resourceSpec = Utility.ResourceSpec {switch, arg, extra, discard, copy, size = Utility.returnIntComp (-1), defaultValues}
      Utility.registerSwitcher (utilityHandle h) O.Clear envTypeName resourceSpec

registerDefaultFunctions ::
  Handle ->
  Context ->
  DD.DefiniteDescription ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  App [C.Value]
registerDefaultFunctions h context name fvs impArgs expArgs defaultArgs =
  case defaultArgs of
    [] ->
      return []
    _ -> do
      fvsAndFixedArgs <- dropFst <$> clarifyBinder h context (fvs ++ impArgs ++ expArgs)
      let fvs' = take (length fvs) fvsAndFixedArgs
      let fixedArgs = drop (length fvs) fvsAndFixedArgs
      let context' = extendContext (fvs ++ impArgs ++ expArgs) context
      envLayout <- closureEnvLayout h fvs
      registerDefaultFunctions' h context' name envLayout fvs' fixedArgs 0 defaultArgs

registerDefaultFunctions' ::
  Handle ->
  Context ->
  DD.DefiniteDescription ->
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  Int ->
  [(BinderF TM.Type, TM.Term)] ->
  App [C.Value]
registerDefaultFunctions' h context name envLayout fvs args index defaultArgs =
  case defaultArgs of
    [] ->
      return []
    (binder@(_, _, x, codType), value) : rest -> do
      let labelName = defaultLabelName name index
      let labelContext = setCurrentFunction labelName context
      codType' <- clarifyType h labelContext codType
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) labelName
      unless isAlreadyRegistered $ do
        body <- clarifyTerm h labelContext value
        liftIO $ registerClosure h labelName O.Clear False codType' (map (const False) args) args envLayout fvs body
      let argNum = AN.fromInt (length args + 2)
      let labelValue = C.VarGlobal labelName argNum (FCT.Cod BLT.Pointer)
      let context' = extendContext [binder] context
      restValues <- registerDefaultFunctions' h context' name envLayout fvs (args ++ [(x, codType')]) (index + 1) rest
      return (labelValue : restValues)

clarifyBinderBody ::
  Handle ->
  Context ->
  [BinderF TM.Type] ->
  TM.Term ->
  App ([(Ident, C.Comp)], C.Comp)
clarifyBinderBody h context xts e =
  case xts of
    [] -> do
      e' <- clarifyTerm h context e
      return ([], e')
    (m, k, x, t) : rest -> do
      t' <- clarifyType h context t
      (binder, e') <- clarifyBinderBody h (extendContext [(m, k, x, t)] context) rest e
      return ((x, t') : binder, e')

clarifyStmtDefineTypeBody ::
  Handle ->
  Context ->
  [(Ident, C.Comp)] ->
  TM.Type ->
  App C.Comp
clarifyStmtDefineTypeBody h context xts t = do
  clarifyType h context t
    >>= liftIO . Linearize.linearize (linearizeHandle h) xts

clarifyStmtDefineBody' ::
  Handle ->
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  App C.CompStmt
clarifyStmtDefineBody' h name xts' dataType = do
  dataType' <- liftIO $ Linearize.linearize (linearizeHandle h) xts' dataType
  return $ C.Def name O.Clear (map fst xts') dataType'

clarifyTerm :: Handle -> Context -> TM.Term -> App C.Comp
clarifyTerm h context term =
  case term of
    _ :< TM.Var x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.VarGlobal (AttrVG.Attr {..}) x -> do
      envType <- liftIO $ envTypeForGlobal h x
      let (globalArgNum, globalCodType) = getGlobalRefInfo AttrVG.Attr {..}
      return $
        C.UpIntro $
          C.SigmaIntro
            (DI.closureLayout (dataSize h))
            [ envType,
              C.null,
              C.VarGlobal x globalArgNum globalCodType
            ]
    _ :< TM.PiIntro attr impArgs expArgs defaultArgs e -> do
      clarifyLambda h context attr (TM.chainOf (typeEnv context) [term]) impArgs expArgs defaultArgs e
    _ :< TM.PiElim _ conv e impArgs expArgs defaultArgs -> do
      conv' <- CC.traverseTypes (clarifyType h context) conv
      impArgs' <- mapM (clarifyTypePlus h context) impArgs
      let allConventions = CC.argumentsFor (length expArgs + length defaultArgs) conv'
      let (expConventions, defaultConventions) = splitAt (length expArgs) allConventions
      expArgs' <- mapM (clarifyPlus h context) expArgs
      defaultArgs' <- mapM (traverse (clarifyPlus h context)) defaultArgs
      let allArgs = impArgs' ++ expArgs' ++ catMaybes defaultArgs'
      case e of
        _ :< TM.Prim (PV.Op op) ->
          return $ callPrimOp op allArgs
        _ -> do
          e' <- clarifyTerm h context e
          liftIO $ callClosure h conv' e' impArgs' (zip expConventions expArgs') (zip defaultConventions defaultArgs')
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consName
      case od of
        Just OD.Enum ->
          return $ C.UpIntro $ C.Int slotIntSize (D.reify discriminant)
        Just OD.Unary
          | [e] <- consArgs ->
              clarifyTerm h context e
          | otherwise ->
              raiseCritical m "Found a malformed unary data in Scene.Clarify.clarifyTerm"
        _ -> do
          (zs1, es1, xs1) <- unzip3 <$> mapM (clarifyTypePlus h context) dataArgs
          (zs2, es2, xs2) <- unzip3 <$> mapM (clarifyPlus h context) consArgs
          dataInfo <- lookupDataEntry h m dataName
          consInfo <- getConsInfoByDiscriminant h m discriminant (DI.consInfoList dataInfo)
          let shape = DI.cellShape (dataSize h) dataInfo
          let layout = DI.consLayout shape consInfo
          let fieldStorageList = DI.consArgLayouts consInfo
          when (length fieldStorageList /= length xs2) $
            raiseCritical m "Found a constructor layout arity mismatch"
          let headerEntries = discriminantEntries shape discriminant
          let dataArgEntries = map (DI.dataArgStorage,) xs1
          let entries = headerEntries ++ dataArgEntries ++ zip fieldStorageList xs2
          packedBody <- liftIO $ Sigma.introCell (sigmaHandle h) layout entries
          return $
            Utility.bindLet (zip zs1 es1 ++ zip zs2 es2) packedBody
    m :< TM.DataElim _ isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (\x -> (m, VK.normal, x, m :< TM.Tau)) xs
      es' <- mapM (clarifyTerm h context) es
      (tree', _) <- clarifyDecisionTree h (extendContext mxts context) isNoetic IntMap.empty tree
      return $ Utility.bindLetWithReducibility False (zip xs es') tree'
    _ :< TM.BoxIntro _ letSeq e -> do
      embody h context letSeq e
    _ :< TM.BoxIntroLift _ e -> do
      clarifyTerm h context e
    _ :< TM.EmbedIntro e -> do
      C.UpIntro <$> clarifyStaticValue h context e
    _ :< TM.BoxElim _ castSeq mxt e1 uncastSeq e2 -> do
      let opaqueLetSeq = map (\(mxt', e) -> (False, mxt', e)) castSeq
      let clearLetSeq = (True, mxt, e1) : map (\(mxt', e) -> (True, mxt', e)) uncastSeq
      clarifyLetSeq h context (opaqueLetSeq ++ clearLetSeq) e2
    _ :< TM.CodeIntro e -> do
      clarifyTerm h context e
    _ :< TM.CodeElim _ e -> do
      clarifyTerm h context e
    _ :< TM.TauIntro ty -> do
      clarifyType h context ty
    _ :< TM.TauElim _ (mx, _, x) e1 e2 -> do
      clarifyLet h context (mx, VK.normal, x, mx :< TM.Tau) e1 e2
    _ :< TM.Let mxt e1 e2 ->
      clarifyLet h context mxt e1 e2
    _ :< TM.Invoke _ body -> do
      clarifyTerm h context body
    m :< TM.Prim primValue ->
      case primValue of
        PV.Int _ size l ->
          return $ C.UpIntro (C.Int size l)
        PV.Float _ size l ->
          return $ C.UpIntro (C.Float size l)
        PV.Op op -> do
          clarifyPrimOp h context op m
        PV.NoeticString _ text ->
          return $ C.UpIntro $ C.VarStaticBytes $ TE.encodeUtf8 text
        PV.NoeticBinary _ bytes ->
          return $ C.UpIntro $ C.VarStaticBytes bytes
        PV.Text text ->
          return $ C.UpIntro $ C.VarStaticBytes $ TE.encodeUtf8 text
        PV.Blob bytes ->
          return $ C.UpIntro $ C.VarStaticBytes bytes
        PV.Rune r -> do
          let t = fromPrimNum m (PT.Int PNS.IntSize32)
          clarifyTerm h context $ m :< TM.Prim (PV.Int t PNS.IntSize32 (RU.asInt r))
    _ :< TM.Magic _ der -> do
      clarifyMagic h context der

clarifyType :: Handle -> Context -> TM.Type -> App C.Comp
clarifyType h context ty =
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
            (DI.closureLayout (dataSize h))
            [ envType,
              C.null,
              C.VarGlobal x globalArgNum globalCodType
            ]
    _ :< TM.TyApp t args -> do
      t' <- clarifyType h context t
      args' <- mapM (clarifyTypePlus h context) args
      liftIO $ callClosure h CC.normal t' args' [] []
    _ :< TM.Pi {} ->
      return Sigma.returnClosureS4
    m :< TM.Data _ name dataArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
      case od of
        Just OD.Enum ->
          return Sigma.returnImmediateS4
        Just OD.Unary -> do
          specializedArgType <- specializeUnaryDataType h m name dataArgs
          clarifyType h context specializedArgType
        _ -> do
          let argNum = AN.fromInt $ length dataArgs + 2
          envType <- liftIO $ envTypeForGlobal h name
          let cls = C.UpIntro $ C.SigmaIntro (DI.closureLayout (dataSize h)) [envType, C.null, C.VarGlobal name argNum (FCT.Cod BLT.Pointer)]
          dataArgs' <- mapM (clarifyTypePlus h context) dataArgs
          liftIO $ callClosure h CC.normal cls dataArgs' [] []
    _ :< TM.Box t -> do
      clarifyType h context t
    _ :< TM.BoxNoema {} ->
      return Sigma.returnImmediateS4
    _ :< TM.Embed {} ->
      return Sigma.returnImmediateS4
    _ :< TM.Code t -> do
      clarifyType h context t
    _ :< TM.PrimType {} ->
      return Sigma.returnImmediateS4
    _ :< TM.Resource dd resourceID -> do
      return $ C.UpIntro $ C.VarGlobal (DD.makeResourceName dd resourceID) AN.argNumS4 (FCT.Cod BLT.Pointer)
    _ :< TM.Void ->
      return Sigma.returnImmediateS4

embody :: Handle -> Context -> [(BinderF TM.Type, TM.Term)] -> TM.Term -> App C.Comp
embody h context xets cont =
  case xets of
    [] ->
      clarifyTerm h context cont
    (mxt@(_, _, x, t), e) : rest -> do
      t' <- clarifyType h context t
      (valueVarName, value, valueVar) <- clarifyPlus h context e
      relApp <- liftIO $ toRelevantAppWith (utilityHandle h) True valueVar t'
      cont' <- embody h (extendContext [mxt] context) rest cont
      cont'' <- liftIO $ Linearize.linearizeUser (linearizeHandle h) [(x, t')] cont'
      return $ Utility.bindLet [(valueVarName, value), (x, relApp)] cont''

clarifyLet :: Handle -> Context -> BinderF TM.Type -> TM.Term -> TM.Term -> App C.Comp
clarifyLet h context mxt e1 e2 = do
  e2' <- clarifyTerm h (extendContext [mxt] context) e2
  clarifyLetBody h context True mxt e1 e2'

clarifyLetSeq :: Handle -> Context -> [(C.IsReducible, BinderF TM.Type, TM.Term)] -> TM.Term -> App C.Comp
clarifyLetSeq h context letSeq cont =
  case letSeq of
    [] ->
      clarifyTerm h context cont
    (isReducible, mxt, e1) : rest -> do
      e2' <- clarifyLetSeq h (extendContext [mxt] context) rest cont
      clarifyLetBody h context isReducible mxt e1 e2'

clarifyLetBody :: Handle -> Context -> C.IsReducible -> BinderF TM.Type -> TM.Term -> C.Comp -> App C.Comp
clarifyLetBody h context isReducible mxt@(_, _, x, _) e1 e2 = do
  mxts' <- dropFst <$> clarifyBinder h context [mxt]
  e2' <- liftIO $ Linearize.linearizeUser (linearizeHandle h) mxts' e2
  e1' <- clarifyTerm h context e1
  return $ Utility.bindLetWithReducibility isReducible [(x, e1')] e2'

type Size =
  Int

type DataArgsMap = IntMap.IntMap ([(Ident, TM.Type)], Size)

fieldStoragesOfConsInfo ::
  Handle ->
  Context ->
  DI.ConsInfo (BinderF TM.Type) ->
  App [Sigma.FieldLayout]
fieldStoragesOfConsInfo h context consInfo = do
  forM (zip (DI.consArgLayouts consInfo) (DI.consArgs consInfo)) $ \(layout, (_, _, _, t)) -> do
    fieldType <- clarifyType h context t
    return $ Sigma.FieldLayout {Sigma.fieldType = fieldType, Sigma.fieldShape = layout}

clarifyStaticValue :: Handle -> Context -> TM.Term -> App C.Value
clarifyStaticValue h context term =
  case term of
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consName
      case od of
        Just OD.Enum ->
          return $ C.Int slotIntSize (D.reify discriminant)
        Just OD.Unary
          | [e] <- consArgs ->
              clarifyStaticValue h context e
          | otherwise ->
              raiseCritical m "Found a malformed unary data in Clarify.clarifyStaticValue"
        _ -> do
          dataArgSlots <- mapM (staticTypeRep h) dataArgs
          consArgSlots <- mapM (clarifyStaticValue h context) consArgs
          dataInfo <- lookupDataEntry h m dataName
          consInfo <- getConsInfoByDiscriminant h m discriminant (DI.consInfoList dataInfo)
          let fieldStorageList = DI.consArgLayouts consInfo
          when (length fieldStorageList /= length consArgSlots) $
            raiseCritical m "Found a constructor layout arity mismatch"
          let shape = DI.cellShape (dataSize h) dataInfo
          let headerEntries = discriminantEntries shape discriminant
          let dataArgEntries = map (DI.dataArgStorage,) dataArgSlots
          staticCell h m (DI.shapeByteSize shape) $ headerEntries ++ dataArgEntries ++ zip fieldStorageList consArgSlots
    m :< TM.PiIntro {} ->
      clarifyStaticClosure h context m term
    m :< TM.VarGlobal {} ->
      clarifyStaticClosure h context m term
    _ :< TM.BoxIntroLift _ e ->
      clarifyStaticValue h context e
    _ :< TM.EmbedIntro e ->
      clarifyStaticValue h context e
    _ :< TM.CodeIntro e ->
      clarifyStaticValue h context e
    _ :< TM.CodeElim _ e ->
      clarifyStaticValue h context e
    _ :< TM.Invoke _ e ->
      clarifyStaticValue h context e
    _ :< TM.Magic _ (M.LowMagic (LM.OpaqueValue e)) ->
      clarifyStaticValue h context e
    _ :< TM.TauIntro ty ->
      staticTypeRep h ty
    m :< TM.Prim primValue ->
      case primValue of
        PV.Int _ size l ->
          return $ C.Int size l
        PV.Float _ size l ->
          return $ C.Float size l
        PV.NoeticString _ text ->
          return $ C.VarStaticBytes $ TE.encodeUtf8 text
        PV.NoeticBinary _ bytes ->
          return $ C.VarStaticBytes bytes
        PV.Text text ->
          return $ C.VarStaticBytes $ TE.encodeUtf8 text
        PV.Blob bytes ->
          return $ C.VarStaticBytes bytes
        PV.Rune r ->
          return $ C.Int PNS.IntSize32 (RU.asInt r)
        PV.Op {} ->
          raiseNonStaticValue m
    m :< _ ->
      raiseNonStaticValue m

clarifyStaticClosure :: Handle -> Context -> Hint -> TM.Term -> App C.Value
clarifyStaticClosure h context m term = do
  closure <- clarifyTerm h context term
  case closure of
    C.UpIntro (C.SigmaIntro layout slots) -> do
      slots' <- mapM (staticSlotOf m) slots
      label <- liftIO $ newStaticDataLabel h
      return $ C.StaticSigmaIntro label layout slots'
    _ ->
      raiseNonStaticValue m

staticSlotOf :: Hint -> C.Value -> App C.Value
staticSlotOf m v =
  case v of
    C.VarGlobal {} ->
      return v
    C.VarStaticBytes {} ->
      return v
    C.StaticSigmaIntro {} ->
      return v
    C.Int {} ->
      return v
    C.Float {} ->
      return v
    C.SigmaIntro _ [] ->
      return v
    _ ->
      raiseNonStaticValue m

staticCell :: Handle -> Hint -> Int -> [(CL.FieldStorage, C.Value)] -> App C.Value
staticCell h m byteSize entries = do
  let offsets = CL.storageOffsets (dataSize h) 0 (map fst entries)
  (slotsList, valuesList) <- unzip <$> zipWithM (staticEntry m) offsets entries
  label <- liftIO $ newStaticDataLabel h
  let layout = CL.CellLayout {CL.cellSlots = concat slotsList, CL.cellByteSize = byteSize}
  return $ C.StaticSigmaIntro label layout $ concat valuesList

staticEntry :: Hint -> Int -> (CL.FieldStorage, C.Value) -> App ([(Int, CL.FieldWidth)], [C.Value])
staticEntry m offset (storage, value) =
  case storage of
    CL.StoredDirect _ ->
      return (CL.storageSlots offset storage, [value])
    CL.StoredFlat chunks ->
      case value of
        C.StaticSigmaIntro _ inner innerValues
          | CL.cellByteSize inner == sum chunks ->
              return (CL.inlineSlots offset inner, innerValues)
        C.SigmaIntro _ []
          | null chunks ->
              return ([], [])
        _ ->
          raiseCritical m "Found an inlined field of an unexpected static layout"

discriminantEntries :: DI.CellShape -> D.Discriminant -> [(CL.FieldStorage, C.Value)]
discriminantEntries shape discriminant =
  case DI.shapeHeader shape of
    Nothing ->
      []
    Just width ->
      [(CL.StoredDirect width, C.Int slotIntSize (D.reify discriminant))]

staticTypeRep :: Handle -> TM.Type -> App C.Value
staticTypeRep h ty =
  case ty of
    _ :< TM.Tau ->
      return Sigma.immediateS4
    _ :< TM.Pi {} ->
      return Sigma.closureS4
    m :< TM.Data _ name dataArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
      case od of
        Just OD.Enum ->
          return Sigma.immediateS4
        Just OD.Unary -> do
          specializedArgType <- specializeUnaryDataType h m name dataArgs
          staticTypeRep h specializedArgType
        _ ->
          return $ C.VarGlobal (DD.getFormDD name) AN.argNumS4 (FCT.Cod BLT.Pointer)
    _ :< TM.Box t ->
      staticTypeRep h t
    _ :< TM.BoxNoema {} ->
      return Sigma.immediateS4
    _ :< TM.Embed {} ->
      return Sigma.immediateS4
    _ :< TM.Code t ->
      staticTypeRep h t
    _ :< TM.PrimType {} ->
      return Sigma.immediateS4
    _ :< TM.Void ->
      return Sigma.immediateS4
    _ :< TM.Resource dd resourceID ->
      return $ C.VarGlobal (DD.makeResourceName dd resourceID) AN.argNumS4 (FCT.Cod BLT.Pointer)
    m :< _ ->
      raiseCritical m "Found an unevaluated type inside an embedded value"

newStaticDataLabel :: Handle -> IO T.Text
newStaticDataLabel h = do
  i <- Gensym.newCount (gensymHandle h)
  return $ "static;" <> T.pack (show i)

raiseNonStaticValue :: Hint -> App a
raiseNonStaticValue m =
  raiseCritical m "Found a non-static term inside an embedded value"

clarifyDataTypeDef ::
  Handle ->
  DD.DefiniteDescription ->
  [BinderF TM.Type] ->
  [DI.ConsInfo (BinderF TM.Type)] ->
  App C.CompStmt
clarifyDataTypeDef h name dataArgs consInfoList = do
  let context = newContext name
  dataArgs' <- dropFst <$> clarifyBinder h context dataArgs
  envArg <- liftIO $ makeEnvArg h
  switchArg <- liftIO $ makeSwitchArg h
  let xts = dataArgs' ++ [envArg, switchArg]
  od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
  case od of
    Just OD.Enum ->
      liftIO (Sigma.returnSigmaEnumS4 (sigmaHandle h) name O.Clear)
        >>= clarifyStmtDefineBody' h name xts
    Just OD.Unary -> do
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, t)]}] -> do
          t' <- clarifyType h (extendContext dataArgs context) t
          return $ C.Def name O.Clear (map fst xts) t'
        _ ->
          raiseCritical' $ "Found a broken unary data metadata for `" <> DD.reify name <> "`"
    _ -> do
      let shape = DI.cellShapeOf (dataSize h) dataArgs consInfoList
      dataInfo' <- mapM (clarifyDataClause h context shape dataArgs) consInfoList
      liftIO (Sigma.returnSigmaDataS4 (sigmaHandle h) name O.Opaque shape dataInfo')
        >>= clarifyStmtDefineBody' h name xts

getDataCellByteSizeFromType :: Handle -> TM.Type -> App Int
getDataCellByteSizeFromType h t =
  case t of
    m :< TM.Data _ dataName _ -> do
      dataInfo <- lookupDataEntry h m dataName
      return $ DI.shapeByteSize $ DI.cellShape (dataSize h) dataInfo
    _ :< TM.TyApp t' _ ->
      getDataCellByteSizeFromType h t'
    m :< TM.TVarGlobal _ dataName -> do
      dataInfo <- lookupDataEntry h m dataName
      return $ DI.shapeByteSize $ DI.cellShape (dataSize h) dataInfo
    m :< _ ->
      raiseCritical m "Clarify.getDataCellByteSizeFromType"

clarifyDataClause ::
  Handle ->
  Context ->
  DI.CellShape ->
  [BinderF TM.Type] ->
  DI.ConsInfo (BinderF TM.Type) ->
  App Sigma.DataConstructorInfo
clarifyDataClause h context shape dataArgsVal consInfo = do
  dataArgs' <- dropFst <$> clarifyBinder h context dataArgsVal
  let context' = extendContext dataArgsVal context
  fieldStorages <- fieldStoragesOfConsInfo h context' consInfo
  let consArgs' = zip (map (\(_, _, x, _) -> x) (DI.consArgs consInfo)) fieldStorages
  return $
    Sigma.DataConstructorInfo
      { Sigma.discriminant = DI.discriminant consInfo,
        Sigma.dataArgs = dataArgs',
        Sigma.consArgs = consArgs',
        Sigma.cellLayout = DI.consLayout shape consInfo
      }

clarifyDecisionTree ::
  Handle ->
  Context ->
  N.IsNoetic ->
  DataArgsMap ->
  DT.DecisionTree TM.Type TM.Term ->
  App (C.Comp, [BinderF TM.Type])
clarifyDecisionTree h context isNoetic dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList letSeq cont@(m :< _) -> do
      let chain = TM.chainOfDecisionTree (typeEnv context) m tree
      cont' <- clarifyTerm h context $ TM.fromLetSeq letSeq cont
      if isNoetic
        then return (cont', chain)
        else do
          (cont'', dataChain) <- tidyCursorList h context dataArgsMap consumedCursorList cont'
          return (cont'', dataChain ++ chain)
    DT.Unreachable -> do
      return (C.Unreachable, [])
    DT.Switch (cursor, cursorType@(m :< _)) (fallbackClause, clauseList) -> do
      (fallbackClause', fallbackChain) <- clarifyDecisionTree h context isNoetic dataArgsMap fallbackClause
      tmp <- mapM (clarifyCase h context isNoetic dataArgsMap cursor cursorType) clauseList
      let (enumCaseList, clauseList', clauseChainList) = unzip3 tmp
      let chain = filter (\(_, _, x, _) -> x /= cursor) $ nubFreeVariables $ fallbackChain ++ concat clauseChainList
      let aligner = alignFreeVariable h context chain
      fallbackClause'' <- aligner fallbackClause'
      clauseList'' <- mapM aligner clauseList'
      let newChain = (m, VK.normal, cursor, m :< TM.Tau) : chain
      let idents = nubOrd $ map (\(_, _, x, _) -> x) newChain
      ck <- getClauseDataGroup h cursorType
      case ck of
        Just OD.Enum -> do
          tree' <- liftIO $ Utility.getEnumElim (utilityHandle h) idents (C.VarLocal cursor) fallbackClause'' (zip enumCaseList clauseList'')
          return (tree', newChain)
        Just OD.Unary -> do
          return (getFirstClause fallbackClause'' clauseList'', newChain)
        _ -> do
          (_, dataInfo) <- lookupDataEntryFromType h m cursorType
          let shape = DI.cellShape (dataSize h) dataInfo
          case DI.shapeHeader shape of
            Nothing ->
              return (getFirstClause fallbackClause'' clauseList'', newChain)
            Just _ -> do
              (disc, discVar) <- liftIO $ Gensym.createVar (gensymHandle h) "disc"
              enumElim <- liftIO $ Utility.getEnumElim (utilityHandle h) idents discVar fallbackClause'' (zip enumCaseList clauseList'')
              return
                ( C.UpElim True disc (C.Primitive (C.Magic (LM.Load (DI.discriminantLoadType shape) (C.VarLocal cursor)))) enumElim,
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

getConsInfoByDiscriminant ::
  Handle ->
  Hint ->
  D.Discriminant ->
  [DI.ConsInfo (BinderF TM.Type)] ->
  App (DI.ConsInfo (BinderF TM.Type))
getConsInfoByDiscriminant h m discriminant consInfoList = do
  case consInfoList of
    [] ->
      raiseCritical m "Could not find constructor metadata by discriminant"
    consInfo : rest -> do
      if DI.discriminant consInfo == discriminant
        then return consInfo
        else getConsInfoByDiscriminant h m discriminant rest

lookupDataEntry :: Handle -> Hint -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataEntry h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      raiseCritical m $ "Could not find constructor metadata for `" <> DD.reify dataName <> "`"

lookupDataEntryFromType ::
  Handle ->
  Hint ->
  TM.Type ->
  App (DD.DefiniteDescription, DI.DataInfo (BinderF TM.Type))
lookupDataEntryFromType h m t =
  case t of
    _ :< TM.Data _ dataName _ -> do
      dataInfo <- lookupDataEntry h m dataName
      return (dataName, dataInfo)
    _ :< TM.TyApp t' _ ->
      lookupDataEntryFromType h m t'
    _ :< TM.TVarGlobal _ dataName -> do
      dataInfo <- lookupDataEntry h m dataName
      return (dataName, dataInfo)
    _ ->
      raiseCritical m "Could not find data metadata from case cursor type"

specializeUnaryDataType ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  [TM.Type] ->
  App TM.Type
specializeUnaryDataType h m dataName dataArgs = do
  dataInfo <- lookupDataEntry h m dataName
  let dataBinders = DI.dataArgs dataInfo
  when (length dataBinders /= length dataArgs) $
    raiseCritical m $
      "Arity mismatch while specializing unary data metadata for `" <> DD.reify dataName <> "`"
  let binderIds = map (\(_, _, x, _) -> x) dataBinders
  let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map Subst.Type dataArgs)
  case DI.consInfoList dataInfo of
    [DI.ConsInfo {DI.consArgs = [(_, _, _, t)]}] ->
      liftIO $ Subst.substType (substHandle h) sub t
    _ ->
      raiseCritical m $ "Found a broken unary data metadata for `" <> DD.reify dataName <> "`"

tidyCursorList :: Handle -> Context -> DataArgsMap -> [Ident] -> C.Comp -> App (C.Comp, [BinderF TM.Type])
tidyCursorList h context dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return (cont, [])
    cursor : rest -> do
      case IntMap.lookup (Ident.toInt cursor) dataArgsMap of
        Nothing ->
          error "tidyCursor"
        Just (dataArgs, cursorSize) -> do
          let (dataArgVars, dataTypes) = unzip dataArgs
          dataTypes' <- mapM (clarifyType h context) dataTypes
          (cont', chain) <- tidyCursorList h context dataArgsMap rest cont
          tmp <- liftIO $ Linearize.linearizeUser (linearizeHandle h) (zip dataArgVars dataTypes') $ do
            C.Free (C.VarLocal cursor) (Just cursorSize) cont'
          let newChain = zipWith (\x t@(m :< _) -> (m, VK.normal, x, t)) dataArgVars dataTypes
          return (tmp, newChain ++ chain)

clarifyCase ::
  Handle ->
  Context ->
  N.IsNoetic ->
  DataArgsMap ->
  Ident ->
  TM.Type ->
  DT.Case TM.Type TM.Term ->
  App (EC.EnumCase, C.Comp, [BinderF TM.Type])
clarifyCase h context isNoetic dataArgsMap cursor cursorType decisionCase = do
  case decisionCase of
    DT.LiteralCase _ l cont -> do
      (body', contChain) <- clarifyDecisionTree h context isNoetic dataArgsMap cont
      case l of
        L.Int i ->
          return (EC.Int i, body', contChain)
        L.Rune r ->
          return (EC.Int (RU.asInt r), body', contChain)
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (_, dataTypes) = unzip dataArgs
      dataArgVars <- liftIO $ mapM (const $ Gensym.newIdentFromText (gensymHandle h) "dataArg") dataTypes
      cursorSize <- getDataCellByteSizeFromType h cursorType
      let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes, cursorSize) dataArgsMap
      let consArgs' = map (\(m, k, x, _) -> (m, k, x, m :< TM.Tau)) consArgs
      let prefixChain = TM.chainOfCaseWithoutCont (typeEnv context) decisionCase
      (body', contChain) <- clarifyDecisionTree h (extendContext consArgs' context) isNoetic dataArgsMap' cont
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
          (_, dataInfo) <- lookupDataEntryFromType h mCons cursorType
          layoutConsInfo <- getConsInfoByDiscriminant h mCons disc (DI.consInfoList dataInfo)
          when (length (DI.consArgs layoutConsInfo) /= length consArgs) $
            raiseCritical mCons "Found a constructor layout arity mismatch"
          let fieldStorageList = DI.consArgLayouts layoutConsInfo
          let consArgIdents = map (\(_, _, x, _) -> x) consArgs
          let shape = DI.cellShape (dataSize h) dataInfo
          let layout = DI.consLayout shape layoutConsInfo
          let fieldEntries = zip consArgIdents fieldStorageList
          headerVars <-
            case DI.shapeHeader shape of
              Nothing ->
                return []
              Just _ -> do
                discriminantVar <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "discriminant"
                return [discriminantVar]
          if isNoetic
            then do
              let firstFieldStart = length headerVars + length dataArgVars
              bindFields <- liftIO $ Sigma.bindFieldsInPlace (sigmaHandle h) (C.VarLocal cursor) layout firstFieldStart fieldEntries body'
              return
                ( EC.Int (D.reify disc),
                  C.SigmaElim False 0 layout (headerVars ++ dataArgVars) (C.VarLocal cursor) bindFields,
                  chain
                )
            else do
              fieldSlotList <- liftIO $ Sigma.makeFieldSlotVars (gensymHandle h) fieldEntries
              let fieldSlotVars = concatMap Sigma.fieldSlotVars fieldSlotList
              let bodyWithFields = Sigma.bindFieldValues fieldSlotList body'
              return
                ( EC.Int (D.reify disc),
                  C.SigmaElim False 0 layout (headerVars ++ dataArgVars ++ fieldSlotVars) (C.VarLocal cursor) bodyWithFields,
                  chain
                )

alignFreeVariable :: Handle -> Context -> [BinderF TM.Type] -> C.Comp -> App C.Comp
alignFreeVariable h context fvs e = do
  fvs' <- dropFst <$> clarifyBinder h context fvs
  liftIO $ Linearize.linearizeUser (linearizeHandle h) fvs' e

clarifyMagic :: Handle -> Context -> M.Magic BLT.BaseLowType TM.Type TM.Term -> App C.Comp
clarifyMagic h context der = do
  case der of
    M.LowMagic magic -> do
      case magic of
        LM.Cast from to value -> do
          (fromVarName, from', fromVar) <- clarifyTypePlus h context from
          (toVarName, to', toVar) <- clarifyTypePlus h context to
          (valueVarName, value', valueVar) <- clarifyPlus h context value
          return $
            Utility.bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
              C.Primitive (C.Magic (LM.Cast fromVar toVar valueVar))
        LM.Store lt unit value pointer -> do
          (unitVarName, unit', unitVar) <- clarifyTypePlus h context unit
          (valueVarName, value', valueVar) <- clarifyPlus h context value
          (pointerVarName, pointer', pointerVar) <- clarifyPlus h context pointer
          return $
            Utility.bindLet [(unitVarName, unit'), (valueVarName, value'), (pointerVarName, pointer')] $
              C.Primitive (C.Magic (LM.Store lt unitVar valueVar pointerVar))
        LM.Load lt pointer -> do
          (pointerVarName, pointer', pointerVar) <- clarifyPlus h context pointer
          return $
            Utility.bindLet [(pointerVarName, pointer')] $
              C.Primitive (C.Magic (LM.Load lt pointerVar))
        LM.Alloca lt size -> do
          (sizeVarName, size', sizeVar) <- clarifyPlus h context size
          return $
            Utility.bindLet [(sizeVarName, size')] $
              C.Primitive (C.Magic (LM.Alloca lt sizeVar))
        LM.External domList cod extFunName args varArgAndTypeList -> do
          (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus h context) args
          let (varArgs, varTypes) = unzip varArgAndTypeList
          (ys, varArgs', ysAsVarArgs) <- unzip3 <$> mapM (clarifyPlus h context) varArgs
          return $
            Utility.bindLet (zip xs args' ++ zip ys varArgs') $
              C.Primitive (C.Magic (LM.External domList cod extFunName xsAsVars (zip ysAsVarArgs varTypes)))
        LM.Global name lt ->
          return $ C.Primitive (C.Magic (LM.Global name lt))
        LM.OpaqueValue e ->
          clarifyTerm h context e
        LM.CallType func arg1 arg2 arg3 -> do
          (funcVarName, func', funcVar) <- clarifyPlus h context func
          (arg1VarName, arg1', arg1Var) <- clarifyPlus h context arg1
          (arg2VarName, arg2', arg2Var) <- clarifyPlus h context arg2
          (arg3VarName, arg3', arg3Var) <- clarifyPlus h context arg3
          return $
            Utility.bindLet [(funcVarName, func'), (arg1VarName, arg1'), (arg2VarName, arg2'), (arg3VarName, arg3')] $
              C.Primitive (C.Magic (LM.CallType funcVar arg1Var arg2Var arg3Var))
    M.Calloc _ num size -> do
      (numVarName, num', numVar) <- clarifyPlus h context num
      (sizeVarName, size', sizeVar) <- clarifyPlus h context size
      return $
        Utility.bindLet [(numVarName, num'), (sizeVarName, size')] $
          C.Primitive (C.Calloc numVar sizeVar)
    M.Malloc _ size -> do
      (sizeVarName, size', sizeVar) <- clarifyPlus h context size
      return $
        Utility.bindLet [(sizeVarName, size')] $
          C.Primitive (C.Alloc sizeVar)
    M.Realloc _ ptr size -> do
      (ptrVarName, ptr', ptrVar) <- clarifyPlus h context ptr
      (sizeVarName, size', sizeVar) <- clarifyPlus h context size
      return $
        Utility.bindLet [(ptrVarName, ptr'), (sizeVarName, size')] $
          C.Primitive (C.Realloc ptrVar sizeVar)
    M.Free _ ptr -> do
      (ptrVarName, ptr', ptrVar) <- clarifyPlus h context ptr
      return $
        Utility.bindLet [(ptrVarName, ptr')] $
          C.Free ptrVar Nothing (C.UpIntro C.null)
    M.InspectType {} ->
      error "InspectType should be evaluated during inline expansion"
    M.EqType {} ->
      error "EqType should be evaluated during inline expansion"
    M.ShowType _ ->
      error "ShowType should be evaluated during inline expansion"
    M.TextCons {} ->
      error "TextCons should be evaluated during inline expansion"
    M.TextUncons _ _ ->
      error "TextUncons should be evaluated during inline expansion"
    M.MakeSwitch {} ->
      error "MakeSwitch should be evaluated during inline expansion"
    M.CompileError {} ->
      error "CompileError should be evaluated during inline expansion"
    M.GetOriginFileName ->
      error "GetOriginFileName should be evaluated during inline expansion"
    M.GetOriginLine ->
      error "GetOriginLine should be evaluated during inline expansion"
    M.GetOriginColumn ->
      error "GetOriginColumn should be evaluated during inline expansion"

clarifyLambda ::
  Handle ->
  Context ->
  AttrL.Attr TM.Type ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [BinderF TM.Type] ->
  [(BinderF TM.Type, TM.Term)] ->
  TM.Term ->
  App C.Comp
clarifyLambda h context attrL@(AttrL.Attr {lamKind}) fvs impArgs expArgs defaultArgs e@(m :< _) = do
  let mxts = impArgs ++ expArgs ++ map fst defaultArgs
  closureID <- liftIO $ freshClosureID h (currentFunction context)
  case lamKind of
    LK.Fix _ isDestPassing (_, _k, recFuncName, codType) -> do
      let liftedName = DD.getMuDD (currentFunction context) recFuncName closureID
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, _, x, _) -> mx :< TM.Var x) appArgs
      let argumentConventions = map argumentConventionOfBinder appArgs
      let baseConv = if isDestPassing then CC.destination codType else CC.normal
      let conv = CC.withArguments argumentConventions baseConv
      let argNum = AN.fromInt $ length appArgs'
      let attr = AttrVG.Attr {argNum, isConstLike = False, isDestPassing}
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
                (m :< TM.PiElim noTrace conv (m :< TM.VarGlobal attr liftedName) [] appArgs' [])
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        liftedBody <- liftIO $ Subst.subst (substHandle h) (IntMap.fromList [(Ident.toInt recFuncName, Subst.Term lamApp)]) e
        let liftedSlots = sourceSlotsOf appArgs
        (liftedArgs, liftedBody') <- clarifyBinderBody h (newContext liftedName) appArgs liftedBody
        envArg <- liftIO $ makeEnvArg h
        switchArg <- liftIO $ makeSwitchArg h
        let suffixParams = [fst envArg, fst switchArg]
        (destParam, liftedBody'') <-
          if isDestPassing
            then do
              codType' <- clarifyType h (extendContext appArgs $ newContext liftedName) codType
              liftIO $ toDestPassing h codType' liftedBody'
            else
              return (Nothing, liftedBody')
        stmt <- liftIO $ defineWithSourceEntries h liftedName O.Opaque destParam liftedSlots liftedArgs liftedArgs suffixParams id liftedBody''
        liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName stmt
      liftIO $ registerDefaultEnvType h liftedName []
      clarifyTerm h context lamApp
    LK.Normal mName isDestPassing codType -> do
      let name = DD.getLambdaDD (currentFunction context) mName closureID
      defaultValues <- registerDefaultFunctions h context name fvs impArgs expArgs defaultArgs
      let slots = sourceSlotsOf mxts
      let lambdaContext = setCurrentFunction name $ extendContext (catMaybes [AttrL.fromAttr attrL] ++ mxts) context
      e' <- clarifyTerm h lambdaContext e
      returnClosure h context closureID mName O.Clear isDestPassing codType fvs mxts slots defaultValues e'

argumentConventionOfBinder :: BinderF TM.Type -> CC.Argument TM.Type
argumentConventionOfBinder (_, k, _, t) =
  if VK.isSource k
    then CC.Source t
    else CC.Plain

clarifyPlus :: Handle -> Context -> TM.Term -> App (Ident, C.Comp, C.Value)
clarifyPlus h context e = do
  e' <- clarifyTerm h context e
  (varName, var) <- liftIO $ Gensym.createVar (gensymHandle h) "var"
  return (varName, e', var)

clarifyResourceSize :: TM.Term -> App C.Comp
clarifyResourceSize resourceSize@(m :< _) = do
  case Resource.layoutOf resourceSize of
    Just Resource.Direct ->
      return $ Utility.returnIntComp (-1)
    Just (Resource.Flattened byteSize) ->
      return $ Utility.returnIntComp (toInteger byteSize)
    Nothing ->
      raiseCritical m "clarifyResourceSize: the resource size was not reduced to an integer"

clarifyTypePlus :: Handle -> Context -> TM.Type -> App (Ident, C.Comp, C.Value)
clarifyTypePlus h context t = do
  t' <- clarifyType h context t
  (varName, var) <- liftIO $ Gensym.createVar (gensymHandle h) "var"
  return (varName, t', var)

clarifyBinder :: Handle -> Context -> [BinderF TM.Type] -> App [(Hint, Ident, C.Comp)]
clarifyBinder h context binder =
  case binder of
    [] ->
      return []
    ((m, k, x, t) : xts) -> do
      t' <- clarifyType h context t
      xts' <- clarifyBinder h (extendContext [(m, k, x, t)] context) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: Handle -> Context -> PrimOp -> Hint -> App C.Comp
clarifyPrimOp h context op m = do
  let (domList, codType) = getTypeInfo op
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- liftIO $ mapAndUnzipM (const (Gensym.createVar (gensymHandle h) "prim")) domList
  let mxts = zipWith (\x t -> (m, VK.normal, x, t)) xs argTypeList
  closureID <- liftIO $ freshClosureID h (currentFunction context)
  returnClosure h context closureID (Just "primOp") O.Clear False (fromPrimNum m codType) [] mxts (map (const False) mxts) [] $ C.Primitive (C.PrimOp op varList)

freshClosureID :: Handle -> DD.DefiniteDescription -> IO Int
freshClosureID h owner = do
  supply <- readIORef (closureNameSupplyRef h)
  let closureID = Map.lookupDefault 0 owner supply
  let supply' = Map.insert owner (closureID + 1) supply
  writeIORef (closureNameSupplyRef h) supply'
  return closureID

returnClosure ::
  Handle ->
  Context ->
  Int ->
  Maybe T.Text ->
  O.Opacity ->
  Bool ->
  TM.Type ->
  [BinderF TM.Type] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Type] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  [Bool] -> -- which parameters are source-passing slots
  [C.Value] -> -- default argument labels
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  App C.Comp
returnClosure h context lamID mName opacity isDestPassing codType fvs xts slots defaultValues e = do
  fvs'' <- dropFst <$> clarifyBinder h context fvs
  xts'' <- dropFst <$> clarifyBinder h context xts
  envLayout <- closureEnvLayout h fvs
  let name = DD.getLambdaDD (currentFunction context) mName lamID
  fvEnvSigma <- liftIO $ Sigma.closureEnvS4 (sigmaHandle h) name envLayout fvs'' defaultValues
  let fvEnv = C.SigmaIntro envLayout (map (C.VarLocal . fst) fvs'')
  let argNum = AN.fromInt $ length xts'' + if isDestPassing then 3 else 2
  isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) name
  unless isAlreadyRegistered $ do
    let codTypeContext = setCurrentFunction name $ extendContext (fvs ++ xts) context
    codType' <- clarifyType h codTypeContext codType
    liftIO $ registerClosure h name opacity isDestPassing codType' slots xts'' envLayout fvs'' e
  return $
    C.UpIntro $
      C.SigmaIntro (DI.closureLayout (dataSize h)) [fvEnvSigma, fvEnv, C.VarGlobal name argNum (FCT.Cod BLT.Pointer)]

closureEnvLayout :: Handle -> [BinderF TM.Type] -> App CL.CellLayout
closureEnvLayout h fvs = do
  widths <- forM fvs $ \(m, _, _, t) -> do
    StorageWidth.storageWidthOf (dataHandle h) (optDataHandle h) (substHandle h) m t
  return $ CL.naturalCell (dataSize h) $ map CL.StoredDirect widths

registerClosure ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  Bool ->
  C.Comp ->
  [Bool] ->
  [(Ident, C.Comp)] ->
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  IO ()
registerClosure h name opacity isDestPassing codType slots xts envLayout fvs e = do
  (envVarName, envVar) <- Gensym.createVar (gensymHandle h) "env"
  (switchVarName, switchVar) <- Gensym.createVar (gensymHandle h) "switch"
  let envEntries = fvs
  (slotNameList, slotVarList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "slot") envEntries
  hole <- Gensym.newIdentFromText (gensymHandle h) "_"
  (destParam, e') <-
    if isDestPassing
      then toDestPassing h codType e
      else return (Nothing, e)
  normalPrefix <- lambdaPrefixNormal h envLayout envEntries slotVarList envVar
  noeticPrefix <- lambdaPrefixNoetic h envLayout envEntries slotVarList envVar
  let allocList =
        map (\slotName -> (slotName, C.Primitive (C.Magic (LM.Alloca BLT.slot (intTerm (1 :: Int)))))) slotNameList
  enumElim <-
    Utility.getEnumElim (utilityHandle h) (envVarName : slotNameList) switchVar normalPrefix [(EC.Int 1, noeticPrefix)]
  let loadList =
        zipWith (\(x, _) slotVar -> (x, C.Primitive (C.Magic (LM.Load BLT.slot slotVar)))) envEntries slotVarList
  let unpackEnv = Utility.bindLet (allocList ++ [(hole, enumElim)] ++ loadList)
  stmt <- defineWithSourceEntries h name opacity destParam slots xts (fvs ++ xts) [envVarName, switchVarName] unpackEnv e'
  AuxEnv.insert (auxEnvHandle h) name stmt

callClosure ::
  Handle ->
  CC.CallConv C.Comp ->
  C.Comp ->
  [(Ident, C.Comp, C.Value)] ->
  [(CC.Argument C.Comp, (Ident, C.Comp, C.Value))] ->
  [(CC.Argument C.Comp, Maybe (Ident, C.Comp, C.Value))] ->
  IO C.Comp
callClosure h kind e impArgs expArgsWithPassings defaultArgsWithPassings = do
  let flag = if CC.isNoetic kind then C.intValue1 else C.intValue0
  let (passings, expArgs) = unzip expArgsWithPassings
  let (impNames, impComps, impVals) = unzip3 impArgs
  let (expNames, expComps, expVals) = unzip3 expArgs
  ((closureVarName, closureVar), envTypeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames h
  defaultTriples <-
    resolveDefaultTriples h envTypeVarName envVar (impVals ++ expVals) defaultArgsWithPassings
  let (defNames, defComps, defVals) = unzip3 defaultTriples
  (slotVals, slotBindings) <- mapAndUnzipM (passSlotArg h) (zip passings expVals)
  let args = impVals ++ slotVals ++ defVals ++ [envVar, flag]
  callComp <- buildCall h kind lamVar args
  return $
    Utility.bindLet [(closureVarName, e)] $
      C.SigmaElim (not $ CC.isNoetic kind) 0 (DI.closureLayout (dataSize h)) [envTypeVarName, envVarName, lamVarName] closureVar $
        Utility.bindLet (zip (impNames ++ expNames ++ defNames) (impComps ++ expComps ++ defComps)) $
          foldr ($) callComp slotBindings

passSlotArg :: Handle -> (CC.Argument C.Comp, C.Value) -> IO (C.Value, C.Comp -> C.Comp)
passSlotArg h (argumentConvention, v) =
  case argumentConvention of
    CC.Plain ->
      return (v, id)
    CC.Source {} -> do
      (sourceName, sourceVar) <- Gensym.createVar (gensymHandle h) "source"
      (resultName, resultVar) <- Gensym.createVar (gensymHandle h) "result"
      let wrap callComp =
            C.UpElim False sourceName (C.UpIntro v) $
              C.UpElim True resultName callComp $
                C.Free sourceVar Nothing (C.UpIntro resultVar)
      return (sourceVar, wrap)

resolveDefaultTriple ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  (Int, Maybe (Ident, C.Comp, C.Value)) ->
  IO (Ident, C.Comp, C.Value)
resolveDefaultTriple h envTypeVarName envVar prefixVals (i, mOverride) =
  case mOverride of
    Just triple ->
      return triple
    Nothing -> do
      (labelName, labelVar) <- Gensym.createVar (gensymHandle h) "label"
      let labelComp = C.PiElimDownElim False (C.VarLocal envTypeVarName) [intTerm (i + 3), C.null, C.null]
      defaultComp <-
        Utility.bindLet [(labelName, labelComp)]
          <$> callDefaultLabel h envTypeVarName envVar prefixVals labelVar
      (defaultName, defaultVar) <- Gensym.createVar (gensymHandle h) "arg"
      return (defaultName, defaultComp, defaultVar)

resolveDefaultTriples ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  [(CC.Argument C.Comp, Maybe (Ident, C.Comp, C.Value))] ->
  IO [(Ident, C.Comp, C.Value)]
resolveDefaultTriples h envTypeVarName envVar fixedVals defaultArgs =
  resolveDefaultTriples' h envTypeVarName envVar fixedVals [] 0 defaultArgs

resolveDefaultTriples' ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  [C.Value] ->
  Int ->
  [(CC.Argument C.Comp, Maybe (Ident, C.Comp, C.Value))] ->
  IO [(Ident, C.Comp, C.Value)]
resolveDefaultTriples' h envTypeVarName envVar fixedVals prevDefaultVals index defaultArgs =
  case defaultArgs of
    [] ->
      return []
    (_, mOverride) : rest -> do
      triple@(_, _, value) <-
        resolveDefaultTriple h envTypeVarName envVar (fixedVals ++ prevDefaultVals) (index, mOverride)
      restTriples <-
        resolveDefaultTriples' h envTypeVarName envVar fixedVals (prevDefaultVals ++ [value]) (index + 1) rest
      return (triple : restTriples)

buildCall ::
  Handle ->
  CC.CallConv C.Comp ->
  C.Value ->
  [C.Value] ->
  IO C.Comp
buildCall h kind lamVar args =
  case CC.destinationType kind of
    Just codType -> do
      sizeComp <- getSizeComp h codType
      return $ C.OutputRequest sizeComp lamVar args
    Nothing ->
      return $ C.PiElimDownElim False lamVar args

intTerm :: (Integral a) => a -> C.Value
intTerm i =
  C.Int slotIntSize (toInteger i)

callDefaultLabel ::
  Handle ->
  Ident ->
  C.Value ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
callDefaultLabel h envTypeVarName envVar prefixVals labelVar = do
  (envCopyName, envCopyVar) <- Gensym.createVar (gensymHandle h) "env"
  let envCopyComp = C.PiElimDownElim False (C.VarLocal envTypeVarName) [intTerm (1 :: Int), envVar, C.null]
  return $
    Utility.bindLet [(envCopyName, envCopyComp)] $
      C.PiElimDownElim False labelVar (prefixVals ++ [envCopyVar, C.intValue0])

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
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
lambdaPrefixNormal h envLayout fvs slotVarList envVar = do
  body <- storeValuesInSlots h (map (C.VarLocal . fst) fvs) slotVarList
  return $ C.SigmaElim True 0 envLayout (map fst fvs) envVar body

lambdaPrefixNoetic ::
  Handle ->
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  C.Value ->
  IO C.Comp
lambdaPrefixNoetic h envLayout fvs slotVarList envVar = do
  -- as == [APP-1, ..., APP-n]
  as <- forM fvs $ \(x, t) -> Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") fvs
  storeBody <- storeValuesInSlots h varList slotVarList
  body <- Linearize.linearize (linearizeHandle h) fvs $ Utility.bindLet (zip varNameList as) storeBody
  return $ C.SigmaElim False 0 envLayout (map fst fvs) envVar body

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
            (\value slotVar -> C.Primitive (C.Magic (LM.Store BLT.slot C.null value slotVar)))
            valueList
            slotVarList
  return $ Utility.bindLet storeBinderList (C.UpIntro C.null)
