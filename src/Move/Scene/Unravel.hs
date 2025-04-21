module Move.Scene.Unravel
  ( unravel,
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
import Move.Context.App.Internal (counter)
import Move.Context.Debug (report)
import Move.Context.EIO (toApp)
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
import Move.Scene.Parse.Core (Handle (Handle))
import Move.Scene.Parse.Core qualified as ParseCore
import Move.Scene.Parse.Import (interpretImport)
import Move.Scene.Parse.Program (parseImport)
import Move.Scene.Source.ShiftToLatest
import Path
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

unravel :: Module -> Target -> App (A.ArtifactTime, [Source.Source])
unravel baseModule t = do
  report "Resolving file dependencies"
  case t of
    Main t' -> do
      case t' of
        Zen path _ ->
          unravelFromFile t baseModule path
        Named targetName _ -> do
          case getTargetPath baseModule targetName of
            Nothing ->
              Throw.raiseError' $ "No such target is defined: `" <> targetName <> "`"
            Just path -> do
              unravelFromFile t baseModule path
    Peripheral -> do
      registerShiftMap
      unravelFoundational t baseModule
    PeripheralSingle path -> do
      unravelFromFile t baseModule path

unravelFromFile ::
  Target ->
  Module ->
  Path Abs File ->
  App (A.ArtifactTime, [Source.Source])
unravelFromFile t baseModule path = do
  Module.sourceFromPath baseModule path >>= unravel' t

unravel' :: Target -> Source.Source -> App (A.ArtifactTime, [Source.Source])
unravel' t source = do
  registerShiftMap
  (artifactTime, sourceSeq) <- unravel'' t source
  let sourceList = toList sourceSeq
  forM_ sourceSeq $ toApp . Parse.ensureExistence
  return (artifactTime, sourceList)

registerShiftMap :: App ()
registerShiftMap = do
  axis <- newAxis
  arrowList <- Env.getMainModule >>= unravelAntecedentArrow axis
  cAxis <- newCAxis
  compressMap cAxis (Map.fromList arrowList) arrowList >>= Antecedent.setMap

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
        counter <- asks counter
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
        b <- Path.doesFileExist path'
        if b
          then do
            counter <- asks counter
            let h = Module.Handle {counter = counter}
            toApp (Module.fromFilePath h path') >>= unravelModule' axis
          else return []
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ currentModule : arrows

unravel'' :: Target -> Source.Source -> App (A.ArtifactTime, Seq Source.Source)
unravel'' t source = do
  visitEnv <- Unravel.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active -> do
      traceSourceList <- Unravel.getTraceSourceList
      raiseCyclicPath path (map Source.sourceFilePath traceSourceList)
    Just VI.Finish -> do
      artifactTime <- Env.lookupArtifactTime path
      return (artifactTime, Seq.empty)
    Nothing -> do
      Unravel.insertToVisitEnv path VI.Active
      Unravel.pushToTraceSourceList source
      children <- getChildren source
      (artifactTimeList, seqList) <- mapAndUnzipM (unravelImportItem t) children
      _ <- Unravel.popFromTraceSourceList
      Unravel.insertToVisitEnv path VI.Finish
      baseArtifactTime <- getBaseArtifactTime t source
      artifactTime <- getArtifactTime artifactTimeList baseArtifactTime
      Env.insertToArtifactMap (Source.sourceFilePath source) artifactTime
      return (artifactTime, foldl' (><) Seq.empty seqList |> source)

unravelImportItem :: Target -> ImportItem -> App (A.ArtifactTime, Seq Source.Source)
unravelImportItem t importItem = do
  case importItem of
    ImportItem source _ ->
      unravel'' t source
    StaticKey staticFileList -> do
      let pathList = map snd staticFileList
      itemModTime <- forM pathList $ \(m, p) -> do
        toApp $ ensureExistence' p (Just m)
        Path.getModificationTime p
      let newestArtifactTime = maximum $ map A.inject itemModTime
      return (newestArtifactTime, Seq.empty)

unravelFoundational :: Target -> Module -> App (A.ArtifactTime, [Source.Source])
unravelFoundational t baseModule = do
  children <- Module.getAllSourceInModule baseModule
  children' <- mapM shiftToLatest children
  (artifactTimeList, seqList) <- mapAndUnzipM (unravel'' t) children'
  baseArtifactTime <- artifactTimeFromCurrentTime
  artifactTime <- getArtifactTime artifactTimeList baseArtifactTime
  return (artifactTime, toList $ foldl' (><) Seq.empty seqList)

getArtifactTime :: [A.ArtifactTime] -> A.ArtifactTime -> App A.ArtifactTime
getArtifactTime artifactTimeList artifactTime = do
  cacheTime <- getItemTime' (map A.cacheTime artifactTimeList) $ A.cacheTime artifactTime
  llvmTime <- getItemTime' (map A.llvmTime artifactTimeList) $ A.llvmTime artifactTime
  objectTime <- getItemTime' (map A.objectTime artifactTimeList) $ A.objectTime artifactTime
  return A.ArtifactTime {cacheTime, llvmTime, objectTime}

getBaseArtifactTime :: Target -> Source.Source -> App A.ArtifactTime
getBaseArtifactTime t source = do
  cacheTime <- getFreshCacheTime t source
  llvmTime <- getFreshLLVMTime t source
  objectTime <- getFreshObjectTime t source
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

getFreshCacheTime :: Target -> Source.Source -> App CacheTime
getFreshCacheTime t source = do
  cachePath <- Path.getSourceCachePath t source
  getFreshTime source cachePath

getFreshLLVMTime :: Target -> Source.Source -> App LLVMTime
getFreshLLVMTime t source = do
  llvmPath <- Path.sourceToOutputPath t OK.LLVM source
  getFreshTime source llvmPath

getFreshObjectTime :: Target -> Source.Source -> App ObjectTime
getFreshObjectTime t source = do
  objectPath <- Path.sourceToOutputPath t OK.Object source
  getFreshTime source objectPath

getFreshTime :: Source.Source -> Path Abs File -> App (Maybe UTCTime)
getFreshTime source itemPath = do
  existsItem <- Path.doesFileExist itemPath
  if not existsItem
    then return Nothing
    else do
      srcModTime <- Path.getModificationTime $ Source.sourceFilePath source
      itemModTime <- Path.getModificationTime itemPath
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
  counter <- asks counter
  let h = Handle {counter, filePath, fileContent, mustParseWholeFile = False}
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

newCAxis :: App CAxis
newCAxis = do
  cacheMapRef <- liftIO $ newIORef Map.empty
  return $ CAxis {..}

compressMap :: CAxis -> ShiftMap -> [(MID.ModuleID, Module)] -> App ShiftMap
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
              Throw.raiseError' $
                "Found a non-confluent antecedent graph:\n"
                  <> MID.reify from
                  <> " ~> {"
                  <> MID.reify (moduleID to')
                  <> ", "
                  <> MID.reify (moduleID to'')
                  <> "}"
        _ ->
          return $ Map.insert from to' restMap

chase :: CAxis -> ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> App Module
chase axis baseMap found k i = do
  cacheMap <- liftIO $ readIORef $ cacheMapRef axis
  case Map.lookup (moduleID i) cacheMap of
    Just j ->
      return j
    Nothing -> do
      chase' axis baseMap found k i

chase' :: CAxis -> ShiftMap -> [MID.ModuleID] -> MID.ModuleID -> Module -> App Module
chase' axis baseMap found k i = do
  case Map.lookup (moduleID i) baseMap of
    Nothing -> do
      liftIO $ modifyIORef' (cacheMapRef axis) $ Map.insert k i
      return i
    Just j -> do
      let j' = moduleID j
      if j' `elem` found
        then
          Throw.raiseError' $
            "Found a cycle in given antecedent graph:\n" <> showCycle (map MID.reify $ j' : found)
        else chase axis baseMap (j' : found) k j

artifactTimeFromCurrentTime :: App A.ArtifactTime
artifactTimeFromCurrentTime = do
  now <- liftIO getCurrentTime
  return
    A.ArtifactTime
      { cacheTime = Just now,
        llvmTime = Just now,
        objectTime = Just now
      }
