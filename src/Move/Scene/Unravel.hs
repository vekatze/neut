module Move.Scene.Unravel
  ( Handle,
    new,
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (Seq, empty, (><), (|>))
import Data.Text qualified as T
import Data.Time
import Move.Context.Alias qualified as Alias
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError', toApp)
import Move.Context.Env (getMainModule)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Module qualified as Module
import Move.Context.Parse (ensureExistence')
import Move.Context.Parse qualified as Parse
import Move.Context.Path qualified as Path
import Move.Context.Throw qualified as Throw
import Move.Context.Unravel qualified as Unravel
import Move.Scene.Module.Reflect qualified as Module
import Move.Scene.Parse.Core qualified as ParseCore
import Move.Scene.Parse.Import (interpretImport)
import Move.Scene.Parse.Program (parseImport)
import Move.Scene.Source.ShiftToLatest
import Path
import Path.IO
import Rule.Artifact qualified as A
import Rule.Hint hiding (new)
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
  { debugHandle :: Debug.Handle,
    pathHandle :: Path.Handle,
    artifactMapRef :: IORef (Map.HashMap (Path Abs File) A.ArtifactTime)
  }

new :: App Handle
new = do
  debugHandle <- Debug.new
  pathHandle <- Path.new
  artifactMapRef <- asks App.artifactMap
  return $ Handle {..}

unravel :: Handle -> Module -> Target -> App (A.ArtifactTime, [Source.Source])
unravel h baseModule t = do
  toApp $ Debug.report (debugHandle h) "Resolving file dependencies"
  case t of
    Main t' -> do
      case t' of
        Zen path _ ->
          unravelFromFile h t baseModule path
        Named targetName _ -> do
          case getTargetPath baseModule targetName of
            Nothing ->
              Throw.raiseError' $ "No such target is defined: `" <> targetName <> "`"
            Just path -> do
              unravelFromFile h t baseModule path
    Peripheral -> do
      registerShiftMap
      unravelFoundational h t baseModule
    PeripheralSingle path -> do
      unravelFromFile h t baseModule path

unravelFromFile ::
  Handle ->
  Target ->
  Module ->
  Path Abs File ->
  App (A.ArtifactTime, [Source.Source])
unravelFromFile h t baseModule path = do
  Module.sourceFromPath baseModule path >>= unravel' h t

unravel' :: Handle -> Target -> Source.Source -> App (A.ArtifactTime, [Source.Source])
unravel' h t source = do
  registerShiftMap
  (artifactTime, sourceSeq) <- unravel'' h t source
  let sourceList = toList sourceSeq
  forM_ sourceSeq $ toApp . Parse.ensureExistence
  return (artifactTime, sourceList)

registerShiftMap :: App ()
registerShiftMap = do
  axis <- newAxis
  arrowList <- Env.getMainModule >>= \(MainModule m) -> unravelAntecedentArrow axis m
  cAxis <- liftIO newCAxis
  toApp (compressMap cAxis (Map.fromList arrowList) arrowList) >>= Antecedent.setMap

type VisitMap =
  Map.HashMap (Path Abs File) VI.VisitInfo

newAxis :: App Axis
newAxis = do
  visitMapRef <- liftIO $ newIORef Map.empty
  traceListRef <- liftIO $ newIORef []
  return Axis {..}

data Axis = Axis
  { visitMapRef :: IORef VisitMap,
    traceListRef :: IORef [Path Abs File]
  }

unravelAntecedentArrow :: Axis -> Module -> App [(MID.ModuleID, Module)]
unravelAntecedentArrow axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  mainModule <- getMainModule
  path <- toApp $ Module.getModuleFilePath mainModule Nothing (moduleID currentModule)
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
        path' <- toApp $ Module.getModuleFilePath mainModule Nothing moduleID
        counter <- asks App.counter
        let h = Module.Handle {counter = counter}
        toApp (Module.fromFilePath h path') >>= unravelAntecedentArrow axis
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ getAntecedentArrow currentModule ++ arrows

unravelModule :: Module -> App [Module]
unravelModule currentModule = do
  axis <- newAxis
  unravelModule' axis currentModule

unravelModule' :: Axis -> Module -> App [Module]
unravelModule' axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  mainModule <- getMainModule
  path <- toApp $ Module.getModuleFilePath mainModule Nothing (moduleID currentModule)
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
        path' <- toApp $ Module.getModuleFilePath mainModule Nothing moduleID
        b <- doesFileExist path'
        if b
          then do
            counter <- asks App.counter
            let h = Module.Handle {counter = counter}
            toApp (Module.fromFilePath h path') >>= unravelModule' axis
          else return []
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ currentModule : arrows

unravel'' :: Handle -> Target -> Source.Source -> App (A.ArtifactTime, Seq Source.Source)
unravel'' h t source = do
  visitEnv <- Unravel.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active -> do
      traceSourceList <- Unravel.getTraceSourceList
      raiseCyclicPath path (map Source.sourceFilePath traceSourceList)
    Just VI.Finish -> do
      artifactTime <- toApp $ Env.lookupArtifactTime (artifactMapRef h) path
      return (artifactTime, Seq.empty)
    Nothing -> do
      Unravel.insertToVisitEnv path VI.Active
      Unravel.pushToTraceSourceList source
      children <- getChildren source
      (artifactTimeList, seqList) <- mapAndUnzipM (unravelImportItem h t) children
      _ <- Unravel.popFromTraceSourceList
      Unravel.insertToVisitEnv path VI.Finish
      baseArtifactTime <- toApp $ getBaseArtifactTime (pathHandle h) t source
      artifactTime <- getArtifactTime artifactTimeList baseArtifactTime
      Env.insertToArtifactMap (Source.sourceFilePath source) artifactTime
      return (artifactTime, foldl' (><) Seq.empty seqList |> source)

unravelImportItem :: Handle -> Target -> ImportItem -> App (A.ArtifactTime, Seq Source.Source)
unravelImportItem h t importItem = do
  case importItem of
    ImportItem source _ ->
      unravel'' h t source
    StaticKey staticFileList -> do
      let pathList = map snd staticFileList
      itemModTime <- forM pathList $ \(m, p) -> do
        toApp $ ensureExistence' p (Just m)
        getModificationTime p
      let newestArtifactTime = maximum $ map A.inject itemModTime
      return (newestArtifactTime, Seq.empty)

unravelFoundational :: Handle -> Target -> Module -> App (A.ArtifactTime, [Source.Source])
unravelFoundational h t baseModule = do
  children <- Module.getAllSourceInModule baseModule
  children' <- mapM shiftToLatest children
  (artifactTimeList, seqList) <- mapAndUnzipM (unravel'' h t) children'
  baseArtifactTime <- liftIO artifactTimeFromCurrentTime
  artifactTime <- getArtifactTime artifactTimeList baseArtifactTime
  return (artifactTime, toList $ foldl' (><) Seq.empty seqList)

getArtifactTime :: [A.ArtifactTime] -> A.ArtifactTime -> App A.ArtifactTime
getArtifactTime artifactTimeList artifactTime = do
  cacheTime <- getItemTime' (map A.cacheTime artifactTimeList) $ A.cacheTime artifactTime
  llvmTime <- getItemTime' (map A.llvmTime artifactTimeList) $ A.llvmTime artifactTime
  objectTime <- getItemTime' (map A.objectTime artifactTimeList) $ A.objectTime artifactTime
  return A.ArtifactTime {cacheTime, llvmTime, objectTime}

getBaseArtifactTime :: Path.Handle -> Target -> Source.Source -> EIO A.ArtifactTime
getBaseArtifactTime h t source = do
  cacheTime <- getFreshCacheTime h t source
  llvmTime <- getFreshLLVMTime h t source
  objectTime <- getFreshObjectTime h t source
  return A.ArtifactTime {cacheTime, llvmTime, objectTime}

getItemTime' ::
  [Maybe UTCTime] ->
  Maybe UTCTime ->
  App (Maybe UTCTime)
getItemTime' mTimeList mTime = do
  case (mTime, distributeMaybe mTimeList) of
    (Nothing, _) ->
      return Nothing
    (_, Nothing) ->
      return Nothing
    (Just time, Just childTimeList) -> do
      if all (time >=) childTimeList
        then return $ Just time
        else return Nothing

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

raiseCyclicPath :: Path Abs File -> [Path Abs File] -> App a
raiseCyclicPath path pathList = do
  let m = newSourceHint path
  let cyclicPathList = reverse $ path : pathList
  Throw.raiseError m $ "Found a cyclic import:\n" <> showCycle (map (T.pack . toFilePath) cyclicPathList)

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

getChildren :: Source.Source -> App [ImportItem]
getChildren currentSource = do
  Env.setCurrentSource currentSource
  Alias.initializeAliasMap
  sourceChildrenMap <- Unravel.getSourceChildrenMap
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceAliasList ->
      return sourceAliasList
    Nothing -> do
      sourceAliasList <- parseSourceHeader currentSource
      Unravel.insertToSourceChildrenMap currentSourceFilePath sourceAliasList
      return sourceAliasList

parseSourceHeader :: Source.Source -> App [ImportItem]
parseSourceHeader currentSource = do
  Locator.initialize
  toApp $ Parse.ensureExistence currentSource
  let filePath = Source.sourceFilePath currentSource
  fileContent <- liftIO $ Parse.readTextFile filePath
  counter <- asks App.counter
  let h = ParseCore.Handle {counter, filePath, fileContent, mustParseWholeFile = False}
  (_, importList) <- toApp $ ParseCore.parseFile h (const parseImport)
  let m = newSourceHint filePath
  interpretImport m currentSource importList

getAntecedentArrow :: Module -> [(MID.ModuleID, Module)]
getAntecedentArrow baseModule = do
  let antecedents = moduleAntecedents baseModule
  map (\antecedent -> (MID.Library antecedent, baseModule)) antecedents

newtype CAxis = CAxis
  { cacheMapRef :: IORef ShiftMap
  }

newCAxis :: IO CAxis
newCAxis = do
  cacheMapRef <- newIORef Map.empty
  return $ CAxis {..}

compressMap :: CAxis -> ShiftMap -> [(MID.ModuleID, Module)] -> EIO ShiftMap
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

chase :: CAxis -> ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> EIO Module
chase axis baseMap found k i = do
  cacheMap <- liftIO $ readIORef $ cacheMapRef axis
  case Map.lookup (moduleID i) cacheMap of
    Just j ->
      return j
    Nothing -> do
      chase' axis baseMap found k i

chase' :: CAxis -> ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> EIO Module
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
