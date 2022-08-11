module Scene.Unravel
  ( unravel,
  )
where

import qualified Context.Env as Env
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Entity.Hint
import Entity.Module
import Entity.OutputKind
import qualified Entity.Source as Source
import Entity.VisitInfo
import Path
import qualified Scene.Parse.Core as ParseCore
import qualified Scene.Parse.Import as Parse

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

-- data Context = Context
--   { asThrowCtx :: Throw.Context,
--     asPathCtx :: Path.Context,
--     asModuleCtx :: Module.Context,
--     traceSourceListRef :: IORef [Source.Source],
--     visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo),
--     sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source.Source]),
--     hasCacheSetRef :: IORef PathSet,
--     hasObjectSetRef :: IORef PathSet,
--     sourceAliasMapRef :: IORef Source.SourceAliasMap,
--     getMainModule :: Module,
--     getMode :: Mode.Mode
--   }

class
  ( Throw.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    Source.Context m,
    Parse.Context m
  ) =>
  Context m
  where
  initialize :: Module -> m ()
  doesFileExist :: Path Abs File -> m Bool
  getModificationTime :: Path Abs File -> m UTCTime

-- traceSourceListRef :: IORef [Source.Source]
-- visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo)
-- sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source.Source])
-- hasObjectSetRef :: IORef PathSet
-- getMainModule :: Module

-- hasCacheSetRef :: IORef PathSet
-- sourceAliasMapRef :: IORef SourceAliasMap

unravel ::
  Context m =>
  Module ->
  Source.Source ->
  m (IsCacheAvailable, IsObjectAvailable, Seq Source.Source)
-- m (IsCacheAvailable, IsObjectAvailable, S.Set (Path Abs File), S.Set (Path Abs File), SourceAliasMap, Seq Source.Source)
unravel mainModule source = do
  initialize mainModule
  unravel' source

-- <- newCtx mode throwCtx pathCtx moduleCtx mainModule
-- (isCacheAvailable, isObjectAvailable, sourceSeq) <- unravel' source
-- sourceAliasMap <- readIORef $ sourceAliasMapRef
-- hasCacheSet <- readIORef $ hasCacheSetRef
-- hasObjectSet <- readIORef $ hasObjectSetRef
-- return (isCacheAvailable, isObjectAvailable, hasCacheSet, hasObjectSet, sourceAliasMap, sourceSeq)

-- newCtx :: Throw.Context m => Path.Context m => Module.Context m => Module -> IO Context
-- newCtx mode throwCtx pathCtx moduleCtx mainModule = do
--   _traceSourceListRef <- newIORef []
--   _visitEnvRef <- newIORef Map.empty
--   _sourceChildrenMapRef <- newIORef Map.empty
--   _sourceAliasMapRef <- newIORef Map.empty
--   _hasCacheSetRef <- newIORef S.empty
--   _hasObjectSetRef <- newIORef S.empty
--   return $
--     Context
--       { asThrowCtx = throwCtx,
--         asModuleCtx = moduleCtx,
--         asPathCtx = pathCtx,
--         traceSourceListRef = _traceSourceListRef,
--         visitEnvRef = _visitEnvRef,
--         sourceChildrenMapRef = _sourceChildrenMapRef,
--         hasCacheSetRef = _hasCacheSetRef,
--         hasObjectSetRef = _hasObjectSetRef,
--         sourceAliasMapRef = _sourceAliasMapRef,
--         getMainModule = mainModule,
--         getMode = mode
--       }

unravel' :: Context m => Source.Source -> m (IsCacheAvailable, IsObjectAvailable, Seq Source.Source)
unravel' source = do
  visitEnv <- Env.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VisitInfoActive ->
      raiseCyclicPath source
    Just VisitInfoFinish -> do
      hasCacheSet <- Env.getHasCacheSet
      hasObjectSet <- Env.getHasObjectSet
      return (path `S.member` hasCacheSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      Env.insertToVisitEnv path VisitInfoActive
      Env.pushToTraceSourceList source
      children <- getChildren source
      (isCacheAvailableList, isObjectAvailableList, seqList) <- unzip3 <$> mapM unravel' children
      _ <- Env.popFromTraceSourceList
      Env.insertToVisitEnv path VisitInfoFinish
      isCacheAvailable <- checkIfCacheIsAvailable isCacheAvailableList source
      isObjectAvailable <- checkIfObjectIsAvailable isObjectAvailableList source
      return (isCacheAvailable, isObjectAvailable, foldl' (><) Seq.empty seqList |> source)

checkIfCacheIsAvailable :: Context m => [IsCacheAvailable] -> Source.Source -> m IsCacheAvailable
checkIfCacheIsAvailable isCacheAvailableList source = do
  b <- isFreshCacheAvailable source
  let isCacheAvailable = and $ b : isCacheAvailableList
  when isCacheAvailable $
    Env.insertToHasObjectSet $ Source.sourceFilePath source
  -- modifyIORef' (hasCacheSetRef) $ S.insert $ sourceFilePath source
  return isCacheAvailable

checkIfObjectIsAvailable :: Context m => [IsObjectAvailable] -> Source.Source -> m IsObjectAvailable
checkIfObjectIsAvailable isObjectAvailableList source = do
  b <- isFreshObjectAvailable source
  let isObjectAvailable = and $ b : isObjectAvailableList
  when isObjectAvailable $
    Env.insertToHasObjectSet $ Source.sourceFilePath source
  -- modifyIORef' (hasObjectSetRef) $ S.insert $ sourceFilePath source
  return isObjectAvailable

isFreshCacheAvailable :: Context m => Source.Source -> m Bool
isFreshCacheAvailable source = do
  cachePath <- Source.getSourceCachePath source
  isItemAvailable source cachePath

isFreshObjectAvailable :: Context m => Source.Source -> m Bool
isFreshObjectAvailable source = do
  objectPath <- Source.sourceToOutputPath OutputKindObject source
  isItemAvailable source objectPath

isItemAvailable :: Context m => Source.Source -> Path Abs File -> m Bool
isItemAvailable source itemPath = do
  existsItem <- doesFileExist itemPath
  if not existsItem
    then return False
    else do
      srcModTime <- getModificationTime $ Source.sourceFilePath source
      itemModTime <- getModificationTime itemPath
      return $ itemModTime > srcModTime

raiseCyclicPath :: Context m => Source.Source -> m a
raiseCyclicPath source = do
  traceSourceList <- Env.getTraceSourceList
  let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
  let cyclicPathList = map Source.sourceFilePath $ reverse $ source : traceSourceList
  Throw.raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

showCyclicPath :: [Path Abs File] -> T.Text
showCyclicPath pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      T.pack (toFilePath path)
    path : ps ->
      "     " <> T.pack (toFilePath path) <> showCyclicPath' ps

showCyclicPath' :: [Path Abs File] -> T.Text
showCyclicPath' pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      "\n  ~> " <> T.pack (toFilePath path)
    path : ps ->
      "\n  ~> " <> T.pack (toFilePath path) <> showCyclicPath' ps

getChildren :: Context m => Source.Source -> m [Source.Source]
getChildren currentSource = do
  sourceChildrenMap <- Env.getSourceChildrenMap
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      let path = Source.sourceFilePath currentSource
      (sourceList, aliasInfoList) <- ParseCore.run Parse.parseImportSequence path
      Env.insertToSourceChildrenMap currentSourceFilePath sourceList
      Env.insertToSourceAliasMap currentSourceFilePath aliasInfoList
      return sourceList

-- newParseContext :: Context m => Source.Source -> IO Parse.Context
-- newParseContext source = do
--   locatorCtx <-
--     Mode.locatorCtx (getMode) $
--       Locator.Config
--         { Locator.mainModule = getMainModule,
--           Locator.throwCtx = asThrowCtx,
--           Locator.currentSource = source,
--           Locator.pathCtx = asPathCtx,
--           Locator.moduleCtx = asModuleCtx
--         }
--   aliasCtx <-
--     Mode.aliasCtx (getMode) $
--       Alias.Config
--         { Alias.currentModule = sourceModule source,
--           Alias.mainModule = getMainModule,
--           Alias.throwCtx = asThrowCtx,
--           Alias.locatorCtx = locatorCtx
--         }
--   return $
--     Parse.Context
--       { Parse.throw = asThrowCtx,
--         Parse.moduleCtx = asModuleCtx,
--         Parse.alias = aliasCtx
--       }
