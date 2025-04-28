module Move.Scene.Unravel
  ( Handle,
    new,
    initialize,
    unravel,
    unravelFromFile,
    registerShiftMap,
    unravel',
    unravelModule,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Sequence as Seq (Seq, empty, (><), (|>))
import Data.Text qualified as T
import Data.Time
import Move.Context.Alias qualified as Alias
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Artifact qualified as Artifact
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError, raiseError')
import Move.Context.Env (getMainModule)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Module qualified as Module
import Move.Context.Parse (ensureExistence')
import Move.Context.Parse qualified as Parse
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Move.Scene.Parse.Core qualified as ParseCore
import Move.Scene.Parse.Import qualified as Import
import Move.Scene.Parse.Program (parseImport)
import Move.Scene.Source.ShiftToLatest qualified as STL
import Path
import Path.IO
import Rule.Artifact qualified as A
import Rule.Hint
import Rule.Import
import Rule.Module
import Rule.ModuleID qualified as MID
import Rule.OutputKind qualified as OK
import Rule.Source qualified as Source
import Rule.Target
import Rule.VisitInfo qualified as VI

type CacheTime =
  Maybe UTCTime

type LLVMTime =
  Maybe UTCTime

type ObjectTime =
  Maybe UTCTime

data Handle
  = Handle
  { envHandle :: Env.Handle,
    debugHandle :: Debug.Handle,
    moduleHandle :: ModuleReflect.Handle,
    pathHandle :: Path.Handle,
    shiftToLatestHandle :: STL.Handle,
    importHandle :: Import.Handle,
    parseHandle :: ParseCore.Handle,
    locatorHandle :: Locator.Handle,
    aliasHandle :: Alias.Handle,
    antecedentHandle :: Antecedent.Handle,
    artifactHandle :: Artifact.Handle,
    visitEnvRef :: IORef (Map.HashMap (Path Abs File) VI.VisitInfo),
    traceSourceListRef :: IORef [Source.Source],
    sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [ImportItem])
  }

new :: Env.Handle -> Gensym.Handle -> App Handle
new envHandle gensymHandle = do
  debugHandle <- Debug.new
  pathHandle <- Path.new
  moduleHandle <- ModuleReflect.new gensymHandle
  shiftToLatestHandle <- STL.new
  importHandle <- Import.new envHandle gensymHandle
  parseHandle <- ParseCore.new gensymHandle
  locatorHandle <- Locator.new
  aliasHandle <- Alias.new
  antecedentHandle <- Antecedent.new
  artifactHandle <- Artifact.new
  visitEnvRef <- asks App.visitEnv
  traceSourceListRef <- asks App.traceSourceList
  sourceChildrenMapRef <- asks App.sourceChildrenMap
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (visitEnvRef h) Map.empty
  writeIORef (sourceChildrenMapRef h) Map.empty

unravel :: Handle -> Module -> Target -> EIO (A.ArtifactTime, [Source.Source])
unravel h baseModule t = do
  Debug.report (debugHandle h) "Resolving file dependencies"
  case t of
    Main t' -> do
      case t' of
        Zen path _ ->
          unravelFromFile h t baseModule path
        Named targetName _ -> do
          case getTargetPath baseModule targetName of
            Nothing ->
              raiseError' $ "No such target is defined: `" <> targetName <> "`"
            Just path -> do
              unravelFromFile h t baseModule path
    Peripheral -> do
      registerShiftMap h
      unravelFoundational h t baseModule
    PeripheralSingle path -> do
      unravelFromFile h t baseModule path

unravelFromFile ::
  Handle ->
  Target ->
  Module ->
  Path Abs File ->
  EIO (A.ArtifactTime, [Source.Source])
unravelFromFile h t baseModule path = do
  Module.sourceFromPath baseModule path >>= unravel' h t

unravel' :: Handle -> Target -> Source.Source -> EIO (A.ArtifactTime, [Source.Source])
unravel' h t source = do
  registerShiftMap h
  (artifactTime, sourceSeq) <- unravel'' h t source
  let sourceList = toList sourceSeq
  forM_ sourceSeq Parse.ensureExistence
  return (artifactTime, sourceList)

registerShiftMap :: Handle -> EIO ()
registerShiftMap h = do
  axis <- liftIO newAxis
  m <- extractModule <$> getMainModule (envHandle h)
  arrowList <- unravelAntecedentArrow h axis m
  cAxis <- liftIO newCAxis
  compressMap cAxis (Map.fromList arrowList) arrowList >>= liftIO . Antecedent.set (antecedentHandle h)

type VisitMap =
  Map.HashMap (Path Abs File) VI.VisitInfo

newAxis :: IO Axis
newAxis = do
  visitMapRef <- liftIO $ newIORef Map.empty
  traceListRef <- liftIO $ newIORef []
  return Axis {..}

data Axis = Axis
  { visitMapRef :: IORef VisitMap,
    traceListRef :: IORef [Path Abs File]
  }

unravelAntecedentArrow :: Handle -> Axis -> Module -> EIO [(MID.ModuleID, Module)]
unravelAntecedentArrow h axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  mainModule <- getMainModule (envHandle h)
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
        ModuleReflect.fromFilePath (moduleHandle h) path' >>= unravelAntecedentArrow h axis
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ getAntecedentArrow currentModule ++ arrows

unravelModule :: Handle -> Module -> EIO [Module]
unravelModule h currentModule = do
  axis <- liftIO newAxis
  unravelModule' h axis currentModule

unravelModule' :: Handle -> Axis -> Module -> EIO [Module]
unravelModule' h axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  mainModule <- getMainModule (envHandle h)
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
          then do
            ModuleReflect.fromFilePath (moduleHandle h) path' >>= unravelModule' h axis
          else return []
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ currentModule : arrows

unravel'' :: Handle -> Target -> Source.Source -> EIO (A.ArtifactTime, Seq Source.Source)
unravel'' h t source = do
  visitEnv <- liftIO $ readIORef (visitEnvRef h)
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active -> do
      traceSourceList <- liftIO $ readIORef (traceSourceListRef h)
      raiseCyclicPath path (map Source.sourceFilePath traceSourceList)
    Just VI.Finish -> do
      artifactTime <- Artifact.lookup (artifactHandle h) path
      return (artifactTime, Seq.empty)
    Nothing -> do
      liftIO $ insertToVisitEnv h path VI.Active
      liftIO $ pushToTraceSourceList h source
      children <- getChildren h source
      (artifactTimeList, seqList) <- mapAndUnzipM (unravelImportItem h t) children
      _ <- liftIO $ popFromTraceSourceList h
      liftIO $ insertToVisitEnv h path VI.Finish
      baseArtifactTime <- getBaseArtifactTime (pathHandle h) t source
      let artifactTime = getArtifactTime artifactTimeList baseArtifactTime
      liftIO $ Artifact.insert (artifactHandle h) (Source.sourceFilePath source) artifactTime
      return (artifactTime, foldl' (><) Seq.empty seqList |> source)

insertToVisitEnv :: Handle -> Path Abs File -> VI.VisitInfo -> IO ()
insertToVisitEnv h k v =
  modifyIORef' (visitEnvRef h) $ Map.insert k v

pushToTraceSourceList :: Handle -> Source.Source -> IO ()
pushToTraceSourceList h source =
  modifyIORef' (traceSourceListRef h) $ (:) source

popFromTraceSourceList :: Handle -> IO ()
popFromTraceSourceList h =
  modifyIORef' (traceSourceListRef h) tail

unravelImportItem :: Handle -> Target -> ImportItem -> EIO (A.ArtifactTime, Seq Source.Source)
unravelImportItem h t importItem = do
  case importItem of
    ImportItem source _ ->
      unravel'' h t source
    StaticKey staticFileList -> do
      let pathList = map snd staticFileList
      itemModTime <- forM pathList $ \(m, p) -> do
        ensureExistence' p (Just m)
        getModificationTime p
      let newestArtifactTime = maximum $ map A.inject itemModTime
      return (newestArtifactTime, Seq.empty)

unravelFoundational :: Handle -> Target -> Module -> EIO (A.ArtifactTime, [Source.Source])
unravelFoundational h t baseModule = do
  children <- Module.getAllSourceInModule baseModule
  children' <- mapM (STL.shiftToLatest (shiftToLatestHandle h)) children
  (artifactTimeList, seqList) <- mapAndUnzipM (unravel'' h t) children'
  baseArtifactTime <- liftIO artifactTimeFromCurrentTime
  let artifactTime = getArtifactTime artifactTimeList baseArtifactTime
  return (artifactTime, toList $ foldl' (><) Seq.empty seqList)

getArtifactTime :: [A.ArtifactTime] -> A.ArtifactTime -> A.ArtifactTime
getArtifactTime artifactTimeList artifactTime = do
  let cacheTime = getItemTime' (map A.cacheTime artifactTimeList) $ A.cacheTime artifactTime
  let llvmTime = getItemTime' (map A.llvmTime artifactTimeList) $ A.llvmTime artifactTime
  let objectTime = getItemTime' (map A.objectTime artifactTimeList) $ A.objectTime artifactTime
  A.ArtifactTime {cacheTime, llvmTime, objectTime}

getBaseArtifactTime :: Path.Handle -> Target -> Source.Source -> EIO A.ArtifactTime
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

getFreshCacheTime :: Path.Handle -> Target -> Source.Source -> EIO CacheTime
getFreshCacheTime h t source = do
  cachePath <- Path.getSourceCachePath h t source
  liftIO $ getFreshTime source cachePath

getFreshLLVMTime :: Path.Handle -> Target -> Source.Source -> EIO LLVMTime
getFreshLLVMTime h t source = do
  llvmPath <- Path.sourceToOutputPath h t OK.LLVM source
  liftIO $ getFreshTime source llvmPath

getFreshObjectTime :: Path.Handle -> Target -> Source.Source -> EIO ObjectTime
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

raiseCyclicPath :: Path Abs File -> [Path Abs File] -> EIO a
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

getChildren :: Handle -> Source.Source -> EIO [ImportItem]
getChildren h currentSource = do
  liftIO $ Env.setCurrentSource (envHandle h) currentSource
  Alias.initializeAliasMap (aliasHandle h)
  sourceChildrenMap <- liftIO $ getSourceChildrenMap h
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceAliasList ->
      return sourceAliasList
    Nothing -> do
      sourceAliasList <- parseSourceHeader h currentSource
      liftIO $ insertToSourceChildrenMap h currentSourceFilePath sourceAliasList
      return sourceAliasList

parseSourceHeader :: Handle -> Source.Source -> EIO [ImportItem]
parseSourceHeader h currentSource = do
  Locator.initialize (locatorHandle h)
  Parse.ensureExistence currentSource
  let filePath = Source.sourceFilePath currentSource
  fileContent <- liftIO $ Parse.readTextFile filePath
  (_, importList) <- ParseCore.parseFile (parseHandle h) filePath fileContent False (const parseImport)
  let m = newSourceHint filePath
  Import.interpretImport (importHandle h) m currentSource importList

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

compressMap :: CAxis -> STL.ShiftMap -> [(MID.ModuleID, Module)] -> EIO STL.ShiftMap
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

chase :: CAxis -> STL.ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> EIO Module
chase axis baseMap found k i = do
  cacheMap <- liftIO $ readIORef $ cacheMapRef axis
  case Map.lookup (moduleID i) cacheMap of
    Just j ->
      return j
    Nothing -> do
      chase' axis baseMap found k i

chase' :: CAxis -> STL.ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> EIO Module
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
