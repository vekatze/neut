module Scene.Unravel
  ( unravel,
  )
where

import qualified Context.Alias as Alias
import qualified Context.Locator as Locator
import qualified Context.Mode as Mode
import qualified Context.Module as Module
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
import Entity.Module
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
    asModuleCtx :: Module.Context,
    traceSourceListRef :: IORef [Source],
    visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo),
    sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source]),
    hasCacheSetRef :: IORef PathSet,
    hasObjectSetRef :: IORef PathSet,
    sourceAliasMapRef :: IORef SourceAliasMap,
    getMainModule :: Module,
    getMode :: Mode.Mode
  }

unravel ::
  Mode.Mode ->
  Throw.Context ->
  Module.Context ->
  Module ->
  Source ->
  IO (IsCacheAvailable, IsObjectAvailable, S.Set (Path Abs File), S.Set (Path Abs File), SourceAliasMap, Seq Source)
unravel mode throwCtx moduleCtx mainModule source = do
  ctx <- newCtx mode throwCtx moduleCtx mainModule
  (isCacheAvailable, isObjectAvailable, sourceSeq) <- unravel' ctx source
  sourceAliasMap <- readIORef $ sourceAliasMapRef ctx
  hasCacheSet <- readIORef $ hasCacheSetRef ctx
  hasObjectSet <- readIORef $ hasObjectSetRef ctx
  return (isCacheAvailable, isObjectAvailable, hasCacheSet, hasObjectSet, sourceAliasMap, sourceSeq)

newCtx :: Mode.Mode -> Throw.Context -> Module.Context -> Module -> IO Context
newCtx mode throwCtx moduleCtx mainModule = do
  _traceSourceListRef <- newIORef []
  _visitEnvRef <- newIORef Map.empty
  _sourceChildrenMapRef <- newIORef Map.empty
  _sourceAliasMapRef <- newIORef Map.empty
  _hasCacheSetRef <- newIORef S.empty
  _hasObjectSetRef <- newIORef S.empty
  return $
    Context
      { asThrowCtx = throwCtx,
        asModuleCtx = moduleCtx,
        traceSourceListRef = _traceSourceListRef,
        visitEnvRef = _visitEnvRef,
        sourceChildrenMapRef = _sourceChildrenMapRef,
        hasCacheSetRef = _hasCacheSetRef,
        hasObjectSetRef = _hasObjectSetRef,
        sourceAliasMapRef = _sourceAliasMapRef,
        getMainModule = mainModule,
        getMode = mode
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
      parseCtx <- newParseContext ctx currentSource
      (sourceList, aliasInfoList) <- run (asThrowCtx ctx) (Parse.parseImportSequence parseCtx) path
      modifyIORef' (sourceChildrenMapRef ctx) $ Map.insert currentSourceFilePath sourceList
      modifyIORef' (sourceAliasMapRef ctx) $ Map.insert currentSourceFilePath aliasInfoList
      return sourceList

newParseContext :: Context -> Source -> IO Parse.Context
newParseContext ctx source = do
  locatorCtx <-
    Mode.locatorCtx (getMode ctx) $
      Locator.Config
        { Locator.mainModule = getMainModule ctx,
          Locator.throwCtx = asThrowCtx ctx,
          Locator.currentSource = source
        }
  aliasCtx <-
    Mode.aliasCtx (getMode ctx) $
      Alias.Config
        { Alias.currentModule = sourceModule source,
          Alias.mainModule = getMainModule ctx,
          Alias.throwCtx = asThrowCtx ctx,
          Alias.locatorCtx = locatorCtx
        }
  return $
    Parse.Context
      { Parse.throw = asThrowCtx ctx,
        Parse.moduleCtx = asModuleCtx ctx,
        Parse.alias = aliasCtx
      }
