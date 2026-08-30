module Kernel.Unravel.Unravel
  ( Handle,
    Result (..),
    new,
    unravel,
    unravelFromFile,
    checkTargetCapabilities,
    registerShiftMap,
    getSourceDependencyMap,
    unravel',
    unravelModule,
  )
where

import App.App (App)
import App.Run (raiseError, raiseError')
import CodeParser.Parser (runParser)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Sequence as Seq (Seq, empty, (><), (|>))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time
import Gensym.CreateHandle qualified as Gensym
import Kernel.Common.Artifact qualified as A
import Kernel.Common.Capability qualified as Capability
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Handle.Global.Env (getMainModule)
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Import
import Kernel.Common.Module
import Kernel.Common.Module.FromPath qualified as ModuleReflect
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Common.SourceDependencyMap (SourceDependencyMap)
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Target
import Kernel.Parse.Internal.Import qualified as Import
import Kernel.Parse.Internal.Program (parseHeader)
import Kernel.Unravel.VisitInfo qualified as VI
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Language.RawTerm.RawStmt (RawRequire (..), RawRequireItem (..))
import SyntaxTree.Series qualified as SE
import Logger.Debug qualified as Logger
import Logger.Hint
import Path
import Path.EnsureFileExistence (ensureFileExistence)
import Path.IO
import Path.Read (readTextFromPath)

type CacheTime =
  Maybe UTCTime

type LLVMTime =
  Maybe UTCTime

type ObjectTime =
  Maybe UTCTime

type PublicEdgeMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)

type PublicReachabilityMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)

data Handle = Handle
  { globalHandle :: Global.Handle,
    visitEnvRef :: IORef (Map.HashMap (Path Abs File) VI.VisitInfo),
    traceSourceListRef :: IORef [Source.Source],
    sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [ImportItem]),
    sourceRequireMapRef :: IORef (Map.HashMap (Path Abs File) [(Hint, Capability.Capability)])
  }

data Result = Result
  { resultArtifactTime :: A.ArtifactTime,
    resultSourceList :: [Source.Source],
    resultLiveSourceSet :: S.Set (Path Abs File)
  }

data Demand
  = ByPlatform
  | ByUniversality

new :: Global.Handle -> IO Handle
new globalHandle = do
  visitEnvRef <- newIORef Map.empty
  traceSourceListRef <- newIORef []
  sourceChildrenMapRef <- newIORef Map.empty
  sourceRequireMapRef <- newIORef Map.empty
  return $ Handle {..}

unravel :: Handle -> Module -> Target -> App Result
unravel h baseModule t = do
  liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) "Resolving file dependencies"
  case t of
    Main t' -> do
      path <- entryPointPath baseModule t'
      unravelFromFile h t baseModule path
    Peripheral -> do
      registerShiftMap h
      unravelFoundational h t baseModule
    PeripheralSingle path -> do
      unravelFromFile h t baseModule path

entryPointPath :: Module -> MainTarget -> App (Path Abs File)
entryPointPath baseModule mainTarget =
  case mainTarget of
    Zen path _ ->
      return path
    Named targetName _ ->
      case getTargetPath baseModule targetName of
        Just path ->
          return path
        Nothing ->
          raiseError' $ "No such target is defined: `" <> targetName <> "`"

checkTargetCapabilities :: Handle -> Module -> MainTarget -> App ()
checkTargetCapabilities h baseModule mainTarget = do
  path <- entryPointPath baseModule mainTarget
  source <- Module.sourceFromPath baseModule path
  void $ checkCapabilities h (Main mainTarget) baseModule [source]

unravelFromFile ::
  Handle ->
  Target ->
  Module ->
  Path Abs File ->
  App Result
unravelFromFile h t baseModule path = do
  Module.sourceFromPath baseModule path >>= unravel' h t baseModule

unravel' :: Handle -> Target -> Module -> Source.Source -> App Result
unravel' h t baseModule source = do
  registerShiftMap h
  (artifactTime, sourceSeq) <- unravel'' h t source
  let sourceList = toList sourceSeq
  forM_ sourceSeq Source.ensureSourceExistence
  reportResolvedSources h sourceList
  liveSourceSet <- checkCapabilities h t baseModule [source]
  return $ Result artifactTime sourceList liveSourceSet

reportResolvedSources ::
  Handle ->
  [Source.Source] ->
  App ()
reportResolvedSources h sourceList = do
  let header = "Resolved " <> T.pack (show $ length sourceList) <> " source files:"
  let body = T.unlines $ map (T.pack . toFilePath . Source.sourceFilePath) sourceList
  liftIO $ Logger.report (Global.loggerHandle (globalHandle h)) $ header <> "\n" <> body

getSourceDependencyMap :: Handle -> [Source.Source] -> IO SourceDependencyMap
getSourceDependencyMap h sourceList = do
  sourceChildrenMap <- getSourceChildrenMap h
  return $ Map.fromList $ map (getSourceDependencies sourceChildrenMap) sourceList

getSourceDependencies :: Map.HashMap (Path Abs File) [ImportItem] -> Source.Source -> (Path Abs File, [Path Abs File])
getSourceDependencies sourceChildrenMap source = do
  let sourcePath = Source.sourceFilePath source
  let children = Map.lookupDefault [] sourcePath sourceChildrenMap
  (sourcePath, mapMaybe getSourceDependency children)

getSourceDependency :: ImportItem -> Maybe (Path Abs File)
getSourceDependency importItem = do
  case importItem of
    ImportItem _ source _ ->
      Just $ Source.sourceFilePath source
    StaticFileKey {} ->
      Nothing

registerShiftMap :: Handle -> App ()
registerShiftMap h = do
  axis <- liftIO newAxis
  let mainModule = getMainModule (Global.envHandle (globalHandle h))
  let m = extractModule mainModule
  arrowList <- unravelAntecedentArrow h axis m
  moduleList <- liftIO $ readIORef $ moduleListRef axis
  cAxis <- liftIO newCAxis
  compressedMap <- compressMap cAxis (Map.fromList arrowList) arrowList
  ensureNoCompatibleDependencyAliasCollision compressedMap moduleList
  let publicReachabilityMap = buildPublicReachabilityMap compressedMap moduleList
  liftIO $ writeIORef (Global.publicModuleReachabilityRef (globalHandle h)) publicReachabilityMap
  liftIO $ Antecedent.set (Global.antecedentHandle (globalHandle h)) compressedMap
  modulePathMap <- ModulePath.build (Global.modulePathHandle (globalHandle h))
  liftIO $ ModulePath.set (Global.modulePathHandle (globalHandle h)) modulePathMap

buildPublicReachabilityMap :: Map.HashMap MID.ModuleID Module -> [Module] -> PublicReachabilityMap
buildPublicReachabilityMap compressedMap moduleList = do
  let edgeMap = Map.fromListWith S.union $ map (publicEdgeEntry compressedMap) moduleList
  closePublicEdgeMap edgeMap

publicEdgeEntry :: Map.HashMap MID.ModuleID Module -> Module -> (MID.ModuleID, S.Set MID.ModuleID)
publicEdgeEntry compressedMap currentModule = do
  let currentModule' = getCanonicalModule compressedMap currentModule
  let publicDependencyIDList = collectPublicDependencyIDs compressedMap currentModule'
  (moduleID currentModule', S.fromList publicDependencyIDList)

closePublicEdgeMap :: PublicEdgeMap -> PublicReachabilityMap
closePublicEdgeMap reachabilityMap = do
  let reachabilityMap' = Map.map (expandPublicReachability reachabilityMap) reachabilityMap
  if reachabilityMap' == reachabilityMap
    then reachabilityMap
    else closePublicEdgeMap reachabilityMap'

expandPublicReachability :: PublicReachabilityMap -> S.Set MID.ModuleID -> S.Set MID.ModuleID
expandPublicReachability reachabilityMap reachableSet =
  S.foldl' (includePublicReachable reachabilityMap) reachableSet reachableSet

includePublicReachable :: PublicReachabilityMap -> S.Set MID.ModuleID -> MID.ModuleID -> S.Set MID.ModuleID
includePublicReachable reachabilityMap reachableSet moduleID =
  S.union reachableSet $ Map.lookupDefault S.empty moduleID reachabilityMap

collectPublicDependencyIDs :: Map.HashMap MID.ModuleID Module -> Module -> [MID.ModuleID]
collectPublicDependencyIDs compressedMap currentModule = do
  let dependencyList = Map.toList $ moduleDependency currentModule
  flip mapMaybe dependencyList $ \(moduleAlias, dependency) -> do
    if MA.isPrivate moduleAlias
      then Nothing
      else do
        let dependencyID = MID.Library $ dependencyDigest dependency
        return $ getCanonicalModuleID compressedMap dependencyID

getCanonicalModule :: Map.HashMap MID.ModuleID Module -> Module -> Module
getCanonicalModule compressedMap currentModule = do
  let currentModuleID = moduleID currentModule
  case Map.lookup currentModuleID compressedMap of
    Just latestModule ->
      latestModule
    Nothing ->
      currentModule

ensureNoCompatibleDependencyAliasCollision :: Map.HashMap MID.ModuleID Module -> [Module] -> App ()
ensureNoCompatibleDependencyAliasCollision compressedMap moduleList =
  forM_ moduleList $ \currentModule -> do
    let dependencyList = Map.toList $ moduleDependency currentModule
    ensureNoDirectCompatibleDependencyAliasCollision' currentModule compressedMap Map.empty dependencyList

ensureNoDirectCompatibleDependencyAliasCollision' ::
  Module ->
  Map.HashMap MID.ModuleID Module ->
  Map.HashMap MID.ModuleID MA.ModuleAlias ->
  [(MA.ModuleAlias, Dependency)] ->
  App ()
ensureNoDirectCompatibleDependencyAliasCollision' currentModule compressedMap seen dependencyList = do
  case dependencyList of
    [] ->
      return ()
    (moduleAlias, dependency) : rest -> do
      let dependencyID = MID.Library $ dependencyDigest dependency
      let canonicalID = getCanonicalModuleID compressedMap dependencyID
      case Map.lookup canonicalID seen of
        Just existingAlias ->
          raiseError' $
            "Dependency aliases `"
              <> MA.reify existingAlias
              <> "` and `"
              <> MA.reify moduleAlias
              <> "` in module `"
              <> MID.reify (moduleID currentModule)
              <> "` point to the same compatible module `"
              <> MID.reify canonicalID
              <> "`"
        Nothing -> do
          let seen' = Map.insert canonicalID moduleAlias seen
          ensureNoDirectCompatibleDependencyAliasCollision' currentModule compressedMap seen' rest

getCanonicalModuleID :: Map.HashMap MID.ModuleID Module -> MID.ModuleID -> MID.ModuleID
getCanonicalModuleID compressedMap dependencyID =
  maybe dependencyID moduleID (Map.lookup dependencyID compressedMap)

type VisitMap =
  Map.HashMap (Path Abs File) VI.VisitInfo

newAxis :: IO Axis
newAxis = do
  visitMapRef <- liftIO $ newIORef Map.empty
  traceListRef <- liftIO $ newIORef []
  moduleListRef <- liftIO $ newIORef []
  return Axis {..}

data Axis = Axis
  { visitMapRef :: IORef VisitMap,
    traceListRef :: IORef [Path Abs File],
    moduleListRef :: IORef [Module]
  }

unravelAntecedentArrow :: Handle -> Axis -> Module -> App [(MID.ModuleID, Module)]
unravelAntecedentArrow h axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  let mainModule = getMainModule (Global.envHandle (globalHandle h))
  path <- Module.getModuleFilePath mainModule Nothing (moduleID currentModule)
  case Map.lookup path visitMap of
    Just VI.Active -> do
      pathList <- liftIO $ readIORef $ traceListRef axis
      raiseCyclicPath path pathList
    Just VI.Finish ->
      return []
    Nothing -> do
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Active
      liftIO $ modifyIORef' (traceListRef axis) $ (:) path
      liftIO $ modifyIORef' (moduleListRef axis) $ (:) currentModule
      let children = map (MID.Library . dependencyDigest . snd) $ Map.toList $ moduleDependency currentModule
      arrows <- fmap concat $ forM children $ \moduleID -> do
        path' <- Module.getModuleFilePath mainModule Nothing moduleID
        ModuleReflect.fromFilePath path' >>= unravelAntecedentArrow h axis
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) (drop 1)
      return $ getAntecedentArrow currentModule ++ arrows

unravelModule :: Handle -> Module -> App [Module]
unravelModule h currentModule = do
  axis <- liftIO newAxis
  unravelModule' h axis currentModule

unravelModule' :: Handle -> Axis -> Module -> App [Module]
unravelModule' h axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  let mainModule = getMainModule (Global.envHandle (globalHandle h))
  path <- Module.getModuleFilePath mainModule Nothing (moduleID currentModule)
  case Map.lookup path visitMap of
    Just VI.Active -> do
      pathList <- liftIO $ readIORef $ traceListRef axis
      raiseCyclicPath path pathList
    Just VI.Finish ->
      return []
    Nothing -> do
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Active
      liftIO $ modifyIORef' (traceListRef axis) $ (:) path
      let children = map (MID.Library . dependencyDigest . snd) $ Map.toList $ moduleDependency currentModule
      arrows <- fmap concat $ forM children $ \moduleID -> do
        path' <- Module.getModuleFilePath mainModule Nothing moduleID
        b <- doesFileExist path'
        if b
          then ModuleReflect.fromFilePath path' >>= unravelModule' h axis
          else return []
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) (drop 1)
      return $ currentModule : arrows

unravel'' :: Handle -> Target -> Source.Source -> App (A.ArtifactTime, Seq Source.Source)
unravel'' h t source = do
  visitEnv <- liftIO $ readIORef (visitEnvRef h)
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active -> do
      traceSourceList <- liftIO $ readIORef (traceSourceListRef h)
      raiseCyclicPath path (map Source.sourceFilePath traceSourceList)
    Just VI.Finish -> do
      artifactTime <- Artifact.lookup (Global.artifactHandle (globalHandle h)) path
      return (artifactTime, Seq.empty)
    Nothing -> do
      liftIO $ insertToVisitEnv h path VI.Active
      liftIO $ pushToTraceSourceList h source
      children <- getChildren h source
      results <- forM children $ \item -> do
        (childArtifactTime, childSeq) <- unravelImportItem h t item
        return (item, childArtifactTime, childSeq)
      _ <- liftIO $ popFromTraceSourceList h
      liftIO $ insertToVisitEnv h path VI.Finish
      baseArtifactTime <- getBaseArtifactTime (Global.pathHandle (globalHandle h)) t source
      let artifactTimeList = [childTime | (_, childTime, _) <- results]
      let liveArtifactTimeList = [childTime | (item, childTime, _) <- results, isItemLiveIn (targetEnvironment h) item]
      let seqList = [childSeq | (_, _, childSeq) <- results]
      let artifactTime = getArtifactTime artifactTimeList liveArtifactTimeList baseArtifactTime
      liftIO $ Artifact.insert (Global.artifactHandle (globalHandle h)) (Source.sourceFilePath source) artifactTime
      return (artifactTime, foldl' (><) Seq.empty seqList |> source)

insertToVisitEnv :: Handle -> Path Abs File -> VI.VisitInfo -> IO ()
insertToVisitEnv h k v =
  modifyIORef' (visitEnvRef h) $ Map.insert k v

pushToTraceSourceList :: Handle -> Source.Source -> IO ()
pushToTraceSourceList h source =
  modifyIORef' (traceSourceListRef h) $ (:) source

popFromTraceSourceList :: Handle -> IO ()
popFromTraceSourceList h =
  modifyIORef' (traceSourceListRef h) (drop 1)

unravelImportItem :: Handle -> Target -> ImportItem -> App (A.ArtifactTime, Seq Source.Source)
unravelImportItem h t importItem = do
  case importItem of
    ImportItem _ source _ ->
      unravel'' h t source
    StaticFileKey staticFileList -> do
      let pathList = map snd staticFileList
      itemModTime <- forM pathList $ \(m, p) -> do
        ensureFileExistence p m
        getModificationTime p
      let newestArtifactTime = maximum $ map A.inject itemModTime
      return (newestArtifactTime, Seq.empty)

unravelFoundational :: Handle -> Target -> Module -> App Result
unravelFoundational h t baseModule = do
  let shiftToLatestHandle = STL.new (Global.antecedentHandle (globalHandle h))
  children <- Module.getAllSourceInModule baseModule
  children' <- mapM (STL.shiftToLatest shiftToLatestHandle) children
  (artifactTimeList, seqList) <- mapAndUnzipM (unravel'' h t) children'
  baseArtifactTime <- liftIO artifactTimeFromCurrentTime
  let artifactTime = getArtifactTime artifactTimeList artifactTimeList baseArtifactTime
  let sourceList = toList $ foldl' (><) Seq.empty seqList
  reportResolvedSources h sourceList
  liveSourceSet <- checkCapabilities h t baseModule children'
  return $ Result artifactTime sourceList liveSourceSet

getArtifactTime :: [A.ArtifactTime] -> [A.ArtifactTime] -> A.ArtifactTime -> A.ArtifactTime
getArtifactTime artifactTimeList liveArtifactTimeList artifactTime = do
  let cacheTime = getItemTime' (map A.cacheTime artifactTimeList) $ A.cacheTime artifactTime
  let llvmTime = getItemTime' (map A.llvmTime liveArtifactTimeList) $ A.llvmTime artifactTime
  let objectTime = getItemTime' (map A.objectTime liveArtifactTimeList) $ A.objectTime artifactTime
  A.ArtifactTime {cacheTime, llvmTime, objectTime}

getBaseArtifactTime :: Path.Handle -> Target -> Source.Source -> App A.ArtifactTime
getBaseArtifactTime h t source = do
  cacheTime <- getFreshCacheTime h t source
  llvmTime <- getFreshLLVMTime h t source
  objectTime <- getFreshObjectTime h t source
  return A.ArtifactTime {cacheTime, llvmTime, objectTime}

getItemTime' ::
  [Maybe UTCTime] ->
  Maybe UTCTime ->
  Maybe UTCTime
getItemTime' mTimeList mTime = do
  case (mTime, distributeMaybe mTimeList) of
    (Nothing, _) ->
      Nothing
    (_, Nothing) ->
      Nothing
    (Just time, Just childTimeList) -> do
      if all (time >=) childTimeList
        then Just time
        else Nothing

distributeMaybe :: [Maybe a] -> Maybe [a]
distributeMaybe xs =
  case xs of
    [] ->
      return []
    my : rest -> do
      y <- my
      rest' <- distributeMaybe rest
      return $ y : rest'

getFreshCacheTime :: Path.Handle -> Target -> Source.Source -> App CacheTime
getFreshCacheTime h t source = do
  cachePath <- Path.getSourceCachePath h t source
  liftIO $ getFreshTime source cachePath

getFreshLLVMTime :: Path.Handle -> Target -> Source.Source -> App LLVMTime
getFreshLLVMTime h t source = do
  llvmPath <- Path.sourceToOutputPath h t OK.LLVM source
  liftIO $ getFreshTime source llvmPath

getFreshObjectTime :: Path.Handle -> Target -> Source.Source -> App ObjectTime
getFreshObjectTime h t source = do
  objectPath <- Path.sourceToOutputPath h t OK.Object source
  liftIO $ getFreshTime source objectPath

getFreshTime :: Source.Source -> Path Abs File -> IO (Maybe UTCTime)
getFreshTime source itemPath = do
  existsItem <- doesFileExist itemPath
  if not existsItem
    then return Nothing
    else do
      srcModTime <- getModificationTime $ Source.sourceFilePath source
      itemModTime <- getModificationTime itemPath
      if itemModTime > srcModTime
        then return $ Just itemModTime
        else return Nothing

raiseCyclicPath :: Path Abs File -> [Path Abs File] -> App a
raiseCyclicPath path pathList = do
  let m = newSourceHint path
  let cyclicPathList = reverse $ path : pathList
  raiseError m $ "Found a cyclic import:\n" <> showCycle (map (T.pack . toFilePath) cyclicPathList)

showCycle :: [T.Text] -> T.Text
showCycle textList =
  case textList of
    [] ->
      ""
    [text] ->
      text
    text : ps ->
      "     " <> text <> showCycle' ps

showCycle' :: [T.Text] -> T.Text
showCycle' textList =
  case textList of
    [] ->
      ""
    [text] ->
      "\n  ~> " <> text
    text : ps ->
      "\n  ~> " <> text <> showCycle' ps

getChildren :: Handle -> Source.Source -> App [ImportItem]
getChildren h currentSource = do
  localHandle <- Local.new (globalHandle h) currentSource
  sourceChildrenMap <- liftIO $ getSourceChildrenMap h
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceAliasList ->
      return sourceAliasList
    Nothing -> do
      sourceAliasList <- parseSourceHeader h localHandle currentSource
      liftIO $ insertToSourceChildrenMap h currentSourceFilePath sourceAliasList
      return sourceAliasList

parseSourceHeader :: Handle -> Local.Handle -> Source.Source -> App [ImportItem]
parseSourceHeader h localHandle currentSource = do
  Source.ensureSourceExistence currentSource
  let filePath = Source.sourceFilePath currentSource
  fileContent <- readTextFromPath filePath
  (_, (importList, requireList)) <- runParser filePath fileContent False parseHeader
  requiredList <- mapM interpretRequireItem $ concatMap (SE.extract . requireItemsOf . fst) requireList
  liftIO $ modifyIORef' (sourceRequireMapRef h) $ Map.insert filePath requiredList
  let m = newSourceHint filePath
  gensymHandle <- liftIO Gensym.createHandle
  let importHandle = Import.new gensymHandle (globalHandle h) localHandle
  Import.interpretImport importHandle m currentSource importList

requireItemsOf :: RawRequire -> SE.Series RawRequireItem
requireItemsOf (RawRequire _ _ requireItems _) =
  requireItems

interpretRequireItem :: RawRequireItem -> App (Hint, Capability.Capability)
interpretRequireItem (RawRequireItem m name) = do
  case Capability.reflect name of
    Just capability ->
      return (m, capability)
    Nothing ->
      raiseError m $ "No such capability exists: `" <> name <> "`"

targetEnvironment :: Handle -> S.Set Capability.Capability
targetEnvironment h =
  Capability.providedBy $ Platform.getSelector $ Global.platformHandle $ globalHandle h

demandList :: Handle -> Target -> Module -> [(Demand, S.Set Capability.Capability)]
demandList h t baseModule =
  if moduleUniversal baseModule
    then map ((,) ByUniversality) Capability.everyProvidedSet
    else case t of
      Main _ ->
        [(ByPlatform, targetEnvironment h)]
      _ ->
        []

checkCapabilities :: Handle -> Target -> Module -> [Source.Source] -> App (S.Set (Path Abs File))
checkCapabilities h t baseModule roots = do
  forM_ (demandList h t baseModule) $ \(demand, environment) ->
    void $ walkReachable h environment (ensureCapabilitiesAreProvided h demand environment) roots
  walkReachable h (targetEnvironment h) (\_ _ -> return ()) roots

walkReachable ::
  Handle ->
  S.Set Capability.Capability ->
  ([Source.Source] -> Source.Source -> App ()) ->
  [Source.Source] ->
  App (S.Set (Path Abs File))
walkReachable h environment visit roots = do
  sourceChildrenMap <- liftIO $ getSourceChildrenMap h
  visitedRef <- liftIO $ newIORef S.empty
  let go trace source = do
        let path = Source.sourceFilePath source
        visited <- liftIO $ readIORef visitedRef
        unless (S.member path visited) $ do
          liftIO $ modifyIORef' visitedRef $ S.insert path
          visit (reverse trace) source
          forM_ (Map.lookupDefault [] path sourceChildrenMap) $ \child ->
            case child of
              ImportItem liveness childSource _
                | isLiveIn environment liveness ->
                    go (source : trace) childSource
              _ ->
                return ()
  mapM_ (go []) roots
  liftIO $ readIORef visitedRef

ensureCapabilitiesAreProvided :: Handle -> Demand -> S.Set Capability.Capability -> [Source.Source] -> Source.Source -> App ()
ensureCapabilitiesAreProvided h demand environment importChain source = do
  requireMap <- liftIO $ readIORef (sourceRequireMapRef h)
  let requiredList = Map.lookupDefault [] (Source.sourceFilePath source) requireMap
  forM_ requiredList $ \(m, capability) -> do
    unless (S.member capability environment) $ do
      let (blamed, trace) = blame h m $ importChain ++ [source]
      modulePathMap <- liftIO $ ModulePath.get (Global.modulePathHandle (globalHandle h))
      traceText <- liftIO $ mapM (ModulePath.renderCanonicalSource modulePathMap) trace
      raiseError blamed $
        renderUnavailable demand (Global.platformHandle (globalHandle h)) capability
          <> renderTrace traceText

blame :: Handle -> Hint -> [Source.Source] -> (Hint, [Source.Source])
blame h m trace = do
  let mainModuleID = moduleID $ extractModule $ getMainModule (Global.envHandle (globalHandle h))
  let isInside source = moduleID (Source.sourceModule source) == mainModuleID
  case span isInside trace of
    (inside@(_ : _), outside@(firstOutside : _))
      | Just importHint <- Source.sourceHint firstOutside ->
          (importHint, last inside : outside)
    _ ->
      (m, [])

renderTrace :: [T.Text] -> T.Text
renderTrace traceText =
  if null traceText
    then ""
    else ":\n" <> showCycle traceText

renderUnavailable :: Demand -> Platform.Handle -> Capability.Capability -> T.Text
renderUnavailable demand platformHandle capability =
  case demand of
    ByPlatform ->
      "`" <> Capability.reify capability <> "` is not available on " <> Platform.getPlatformText platformHandle
    ByUniversality ->
      "A universal module cannot require `" <> Capability.reify capability <> "`"

getSourceChildrenMap :: Handle -> IO (Map.HashMap (Path Abs File) [ImportItem])
getSourceChildrenMap h =
  readIORef (sourceChildrenMapRef h)

insertToSourceChildrenMap :: Handle -> Path Abs File -> [ImportItem] -> IO ()
insertToSourceChildrenMap h k v =
  modifyIORef' (sourceChildrenMapRef h) $ Map.insert k v

getAntecedentArrow :: Module -> [(MID.ModuleID, Module)]
getAntecedentArrow baseModule = do
  let antecedents = moduleAntecedents baseModule
  map (\antecedent -> (MID.Library antecedent, baseModule)) antecedents

newtype CAxis = CAxis
  { cacheMapRef :: IORef STL.ShiftMap
  }

newCAxis :: IO CAxis
newCAxis = do
  cacheMapRef <- newIORef Map.empty
  return $ CAxis {..}

compressMap :: CAxis -> STL.ShiftMap -> [(MID.ModuleID, Module)] -> App STL.ShiftMap
compressMap axis baseMap arrowList =
  case arrowList of
    [] ->
      return Map.empty
    (from, to) : rest -> do
      restMap <- compressMap axis baseMap rest
      to' <- chase axis baseMap [] (moduleID to) to
      case Map.lookup from restMap of
        Just to''
          | moduleID to' /= moduleID to'' -> do
              raiseError' $
                "Found a non-confluent antecedent graph:\n"
                  <> MID.reify from
                  <> " ~> {"
                  <> MID.reify (moduleID to')
                  <> ", "
                  <> MID.reify (moduleID to'')
                  <> "}"
        _ ->
          return $ Map.insert from to' restMap

chase :: CAxis -> STL.ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> App Module
chase axis baseMap found k i = do
  cacheMap <- liftIO $ readIORef $ cacheMapRef axis
  case Map.lookup (moduleID i) cacheMap of
    Just j ->
      return j
    Nothing -> do
      chase' axis baseMap found k i

chase' :: CAxis -> STL.ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> App Module
chase' axis baseMap found k i = do
  case Map.lookup (moduleID i) baseMap of
    Nothing -> do
      liftIO $ modifyIORef' (cacheMapRef axis) $ Map.insert k i
      return i
    Just j -> do
      let j' = moduleID j
      if j' `elem` found
        then
          raiseError' $
            "Found a cycle in given antecedent graph:\n" <> showCycle (map MID.reify $ j' : found)
        else chase axis baseMap (j' : found) k j

artifactTimeFromCurrentTime :: IO A.ArtifactTime
artifactTimeFromCurrentTime = do
  now <- getCurrentTime
  return
    A.ArtifactTime
      { cacheTime = Just now,
        llvmTime = Just now,
        objectTime = Just now
      }
