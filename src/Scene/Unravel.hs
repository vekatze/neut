module Scene.Unravel
  ( unravel,
    Context,
  )
where

import Context.Env qualified as Env
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Hint
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.VisitInfo qualified as VI
import Path
import Scene.Parse.Core qualified as ParseCore
import Scene.Parse.Import qualified as Parse

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

class
  ( Throw.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    Source.Context m,
    Parse.Context m
  ) =>
  Context m

unravel ::
  Context m =>
  Source.Source ->
  m (IsCacheAvailable, IsObjectAvailable, Seq Source.Source)
unravel source = do
  unravel' source

unravel' :: Context m => Source.Source -> m (IsCacheAvailable, IsObjectAvailable, Seq Source.Source)
unravel' source = do
  visitEnv <- Env.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active ->
      raiseCyclicPath source
    Just VI.Finish -> do
      hasCacheSet <- Env.getHasCacheSet
      hasObjectSet <- Env.getHasObjectSet
      return (path `S.member` hasCacheSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      Env.insertToVisitEnv path VI.Active
      Env.pushToTraceSourceList source
      children <- getChildren source
      (isCacheAvailableList, isObjectAvailableList, seqList) <- unzip3 <$> mapM unravel' children
      _ <- Env.popFromTraceSourceList
      Env.insertToVisitEnv path VI.Finish
      isCacheAvailable <- checkIfCacheIsAvailable isCacheAvailableList source
      isObjectAvailable <- checkIfObjectIsAvailable isObjectAvailableList source
      return (isCacheAvailable, isObjectAvailable, foldl' (><) Seq.empty seqList |> source)

checkIfCacheIsAvailable :: Context m => [IsCacheAvailable] -> Source.Source -> m IsCacheAvailable
checkIfCacheIsAvailable isCacheAvailableList source = do
  b <- isFreshCacheAvailable source
  let isCacheAvailable = and $ b : isCacheAvailableList
  when isCacheAvailable $
    Env.insertToHasCacheSet $
      Source.sourceFilePath source
  return isCacheAvailable

checkIfObjectIsAvailable :: Context m => [IsObjectAvailable] -> Source.Source -> m IsObjectAvailable
checkIfObjectIsAvailable isObjectAvailableList source = do
  b <- isFreshObjectAvailable source
  let isObjectAvailable = and $ b : isObjectAvailableList
  when isObjectAvailable $
    Env.insertToHasObjectSet $
      Source.sourceFilePath source
  return isObjectAvailable

isFreshCacheAvailable :: Context m => Source.Source -> m Bool
isFreshCacheAvailable source = do
  cachePath <- Source.getSourceCachePath source
  isItemAvailable source cachePath

isFreshObjectAvailable :: Context m => Source.Source -> m Bool
isFreshObjectAvailable source = do
  objectPath <- Source.sourceToOutputPath OK.Object source
  isItemAvailable source objectPath

isItemAvailable :: Context m => Source.Source -> Path Abs File -> m Bool
isItemAvailable source itemPath = do
  existsItem <- Path.doesFileExist itemPath
  if not existsItem
    then return False
    else do
      srcModTime <- Path.getModificationTime $ Source.sourceFilePath source
      itemModTime <- Path.getModificationTime itemPath
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
