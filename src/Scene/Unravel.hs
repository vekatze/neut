module Scene.Unravel
  ( unravel,
    unravelFromFile,
    unravel',
  )
where

import Context.Alias qualified as Alias
import Context.Antecedent qualified as Antecedent
import Context.App
import Context.Env qualified as Env
import Context.Locator qualified as Locator
import Context.Module qualified as Module
import Context.Parse qualified as Parse
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (Seq, empty, (><), (|>))
import Data.Text qualified as T
import Data.Time
import Entity.AliasInfo
import Entity.Artifact qualified as A
import Entity.Hint
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target
import Entity.VisitInfo qualified as VI
import Path
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as ParseCore
import Scene.Parse.Import (interpretImport)
import Scene.Parse.Program (parseImport)
import Scene.Source.ShiftToLatest

type CacheTime =
  Maybe UTCTime

type LLVMTime =
  Maybe UTCTime

type AsmTime =
  Maybe UTCTime

type ObjectTime =
  Maybe UTCTime

unravel :: Target -> App (A.ArtifactTime, [Source.Source])
unravel target = do
  mainModule <- Module.getMainModule
  case getTargetPath mainModule target of
    Nothing ->
      Throw.raiseError' $ "no such target is defined: `" <> extract target <> "`"
    Just mainFilePath -> do
      unravel' $
        Source.Source
          { Source.sourceModule = mainModule,
            Source.sourceFilePath = mainFilePath,
            Source.sourceHint = Nothing
          }

unravelFromFile ::
  Path Abs File ->
  App (A.ArtifactTime, [Source.Source])
unravelFromFile path = do
  Module.sourceFromPath path >>= unravel'

unravel' :: Source.Source -> App (A.ArtifactTime, [Source.Source])
unravel' source = do
  axis <- newAxis
  arrowList <- unravelModule axis (Source.sourceModule source)
  cAxis <- newCAxis
  shiftMap <- compressMap cAxis (Map.fromList arrowList) arrowList
  (artifactTime, sourceSeq) <- unravel'' (UAxis {shiftMap}) source
  Antecedent.setMap shiftMap
  forM_ sourceSeq Parse.ensureExistence
  return (artifactTime, toList sourceSeq)

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

unravelModule :: Axis -> Module -> App [(MID.ModuleID, Module)]
unravelModule axis currentModule = do
  visitMap <- liftIO $ readIORef $ visitMapRef axis
  path <- Module.getModuleFilePath Nothing (moduleID currentModule)
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
        path' <- Module.getModuleFilePath Nothing moduleID
        Module.fromFilePath moduleID path' >>= unravelModule axis
      liftIO $ modifyIORef' (visitMapRef axis) $ Map.insert path VI.Finish
      liftIO $ modifyIORef' (traceListRef axis) tail
      return $ getAntecedentArrow currentModule ++ arrows

newtype UAxis = UAxis
  { shiftMap :: ShiftMap
  }

unravel'' :: UAxis -> Source.Source -> App (A.ArtifactTime, Seq Source.Source)
unravel'' axis source = do
  source' <- shiftToLatest (shiftMap axis) source
  visitEnv <- Unravel.getVisitEnv
  let path = Source.sourceFilePath source'
  case Map.lookup path visitEnv of
    Just VI.Active -> do
      traceSourceList <- Unravel.getTraceSourceList
      raiseCyclicPath path (map Source.sourceFilePath traceSourceList)
    Just VI.Finish -> do
      artifactTime <- Env.lookupArtifactTime path
      return (artifactTime, Seq.empty)
    Nothing -> do
      Unravel.insertToVisitEnv path VI.Active
      Unravel.pushToTraceSourceList source'
      children <- getChildren source'
      (artifactTimeList, seqList) <- mapAndUnzipM (unravel'' axis) children
      _ <- Unravel.popFromTraceSourceList
      Unravel.insertToVisitEnv path VI.Finish
      artifactTime <- getArtifactTime artifactTimeList source'
      return (artifactTime, foldl' (><) Seq.empty seqList |> source')

getArtifactTime :: [A.ArtifactTime] -> Source.Source -> App A.ArtifactTime
getArtifactTime artifactTimeList source = do
  cacheTime <- getCacheTime (map A.cacheTime artifactTimeList) source
  llvmTime <- getLLVMTime (map A.llvmTime artifactTimeList) source
  asmTime <- getAsmTime (map A.asmTime artifactTimeList) source
  objectTime <- getObjectTime (map A.objectTime artifactTimeList) source
  let artifactTime =
        A.ArtifactTime
          { cacheTime = cacheTime,
            llvmTime = llvmTime,
            asmTime = asmTime,
            objectTime = objectTime
          }
  Env.insertToArtifactMap (Source.sourceFilePath source) artifactTime
  return artifactTime

getCacheTime :: [CacheTime] -> Source.Source -> App CacheTime
getCacheTime = do
  getItemTime getFreshCacheTime

getLLVMTime :: [LLVMTime] -> Source.Source -> App LLVMTime
getLLVMTime = do
  getItemTime getFreshLLVMTime

getAsmTime :: [AsmTime] -> Source.Source -> App AsmTime
getAsmTime = do
  getItemTime getFreshAsmTime

getObjectTime :: [ObjectTime] -> Source.Source -> App ObjectTime
getObjectTime = do
  getItemTime getFreshObjectTime

getItemTime ::
  (Source.Source -> App (Maybe UTCTime)) ->
  [Maybe UTCTime] ->
  Source.Source ->
  App (Maybe UTCTime)
getItemTime relatedTimeGetter mTimeList source = do
  mTime <- relatedTimeGetter source
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

getFreshCacheTime :: Source.Source -> App CacheTime
getFreshCacheTime source = do
  cachePath <- Path.getSourceCachePath source
  getFreshTime source cachePath

getFreshLLVMTime :: Source.Source -> App LLVMTime
getFreshLLVMTime source = do
  llvmPath <- Path.sourceToOutputPath OK.LLVM source
  getFreshTime source llvmPath

getFreshAsmTime :: Source.Source -> App AsmTime
getFreshAsmTime source = do
  asmPath <- Path.sourceToOutputPath OK.Asm source
  getFreshTime source asmPath

getFreshObjectTime :: Source.Source -> App ObjectTime
getFreshObjectTime source = do
  objectPath <- Path.sourceToOutputPath OK.Object source
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
  Throw.raiseError m $ "found a cyclic inclusion:\n" <> showCycle (map (T.pack . toFilePath) cyclicPathList)

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

getChildren :: Source.Source -> App [Source.Source]
getChildren currentSource = do
  Env.setCurrentSource currentSource
  Alias.initializeAliasMap
  sourceChildrenMap <- Unravel.getSourceChildrenMap
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceAliasList ->
      return $ map fst sourceAliasList
    Nothing -> do
      sourceAliasList <- parseSourceHeader currentSource
      Unravel.insertToSourceChildrenMap currentSourceFilePath sourceAliasList
      return $ map fst sourceAliasList

parseSourceHeader :: Source.Source -> App [(Source.Source, [AliasInfo])]
parseSourceHeader currentSource = do
  Locator.initialize
  Parse.ensureExistence currentSource
  let path = Source.sourceFilePath currentSource
  fileContent <- Parse.readTextFile path
  (_, (importOrNone, _)) <- ParseCore.parseFile False parseImport path fileContent
  let m = newSourceHint path
  interpretImport m currentSource importOrNone

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
                "found a non-confluent antecedent graph:\n"
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
            "found a cycle in given antecedent graph:\n" <> showCycle (map MID.reify $ j' : found)
        else chase axis baseMap (j' : found) k j
