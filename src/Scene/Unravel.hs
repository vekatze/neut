module Scene.Unravel
  ( unravel,
  )
where

import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Hint
import Entity.OutputKind
import Entity.Source
import Entity.Stmt
import Path
import Path.IO
import Scene.Parse.Core
import qualified Scene.Parse.Import as Parse

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

data Context = Context
  { asThrowCtx :: Throw.Context,
    asPathCtx :: Path.Context,
    traceSourceListRef :: IORef [Source],
    visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo),
    sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source]),
    hasCacheSetRef :: IORef PathSet,
    hasObjectSetRef :: IORef PathSet,
    sourceAliasMapRef :: IORef SourceAliasMap
  }

unravel ::
  Throw.Context ->
  Path.Context ->
  Source ->
  IO (IsCacheAvailable, IsObjectAvailable, S.Set (Path Abs File), S.Set (Path Abs File), SourceAliasMap, Seq Source)
unravel throwCtx pathCtx source = do
  ctx <- newCtx throwCtx pathCtx
  (isCacheAvailable, isObjectAvailable, sourceSeq) <- unravel' ctx source
  sourceAliasMap <- readIORef $ sourceAliasMapRef ctx
  hasCacheSet <- readIORef $ hasCacheSetRef ctx
  hasObjectSet <- readIORef $ hasObjectSetRef ctx
  return (isCacheAvailable, isObjectAvailable, hasCacheSet, hasObjectSet, sourceAliasMap, sourceSeq)

newCtx :: Throw.Context -> Path.Context -> IO Context
newCtx throwCtx pathCtx = do
  _traceSourceListRef <- newIORef []
  _visitEnvRef <- newIORef Map.empty
  _sourceChildrenMapRef <- newIORef Map.empty
  _sourceAliasMapRef <- newIORef Map.empty
  _hasCacheSetRef <- newIORef S.empty
  _hasObjectSetRef <- newIORef S.empty
  return $
    Context
      { asThrowCtx = throwCtx,
        asPathCtx = pathCtx,
        traceSourceListRef = _traceSourceListRef,
        visitEnvRef = _visitEnvRef,
        sourceChildrenMapRef = _sourceChildrenMapRef,
        hasCacheSetRef = _hasCacheSetRef,
        hasObjectSetRef = _hasObjectSetRef,
        sourceAliasMapRef = _sourceAliasMapRef
      }

unravel' :: Context -> Source -> IO (IsCacheAvailable, IsObjectAvailable, Seq Source)
unravel' ctx source = do
  visitEnv <- readIORef $ visitEnvRef ctx
  let path = sourceFilePath source
  case Map.lookup path visitEnv of
    Just VisitInfoActive ->
      raiseCyclicPath ctx source
    Just VisitInfoFinish -> do
      hasCacheSet <- readIORef $ hasCacheSetRef ctx
      hasObjectSet <- readIORef $ hasObjectSetRef ctx
      return (path `S.member` hasCacheSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      modifyIORef' (visitEnvRef ctx) $ Map.insert path VisitInfoActive
      modifyIORef' (traceSourceListRef ctx) $ \sourceList -> source : sourceList
      children <- getChildren ctx source
      (isCacheAvailableList, isObjectAvailableList, seqList) <- unzip3 <$> mapM (unravel' ctx) children
      modifyIORef' (traceSourceListRef ctx) tail
      modifyIORef' (visitEnvRef ctx) $ Map.insert path VisitInfoFinish
      isCacheAvailable <- checkIfCacheIsAvailable ctx isCacheAvailableList source
      isObjectAvailable <- checkIfObjectIsAvailable ctx isObjectAvailableList source
      return (isCacheAvailable, isObjectAvailable, foldl' (><) Seq.empty seqList |> source)

checkIfCacheIsAvailable :: Context -> [IsCacheAvailable] -> Source -> IO IsCacheAvailable
checkIfCacheIsAvailable ctx isCacheAvailableList source = do
  b <- isFreshCacheAvailable source
  let isCacheAvailable = and $ b : isCacheAvailableList
  when isCacheAvailable $
    modifyIORef' (hasCacheSetRef ctx) $ S.insert $ sourceFilePath source
  return isCacheAvailable

checkIfObjectIsAvailable :: Context -> [IsObjectAvailable] -> Source -> IO IsObjectAvailable
checkIfObjectIsAvailable ctx isObjectAvailableList source = do
  b <- isFreshObjectAvailable source
  let isObjectAvailable = and $ b : isObjectAvailableList
  when isObjectAvailable $
    modifyIORef' (hasObjectSetRef ctx) $ S.insert $ sourceFilePath source
  return isObjectAvailable

isFreshCacheAvailable :: Source -> IO Bool
isFreshCacheAvailable source = do
  cachePath <- getSourceCachePath source
  isItemAvailable source cachePath

isFreshObjectAvailable :: Source -> IO Bool
isFreshObjectAvailable source = do
  objectPath <- sourceToOutputPath OutputKindObject source
  isItemAvailable source objectPath

isItemAvailable :: Source -> Path Abs File -> IO Bool
isItemAvailable source itemPath = do
  existsItem <- doesFileExist itemPath
  if not existsItem
    then return False
    else do
      srcModTime <- getModificationTime $ sourceFilePath source
      itemModTime <- getModificationTime itemPath
      return $ itemModTime > srcModTime

raiseCyclicPath :: Context -> Source -> IO a
raiseCyclicPath ctx source = do
  traceSourceList <- readIORef $ traceSourceListRef ctx
  let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
  let cyclicPathList = map sourceFilePath $ reverse $ source : traceSourceList
  Throw.raiseError (asThrowCtx ctx) m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

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

getChildren :: Context -> Source -> IO [Source]
getChildren ctx currentSource = do
  sourceChildrenMap <- readIORef $ sourceChildrenMapRef ctx
  let currentSourceFilePath = sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      let path = sourceFilePath currentSource
      let parseCtx = Parse.Context {Parse.throw = asThrowCtx ctx, Parse.path = asPathCtx ctx}
      (sourceList, aliasInfoList) <-
        run
          (asThrowCtx ctx)
          (Parse.parseImportSequence parseCtx (sourceModule currentSource))
          path
      modifyIORef' (sourceChildrenMapRef ctx) $ Map.insert currentSourceFilePath sourceList
      modifyIORef' (sourceAliasMapRef ctx) $ Map.insert currentSourceFilePath aliasInfoList
      return sourceList
