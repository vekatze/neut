module Context.Unravel
  ( getVisitEnv,
    clearVisitEnv,
    insertToVisitEnv,
    pushToTraceSourceList,
    popFromTraceSourceList,
    getTraceSourceList,
    getSourceChildrenMap,
    insertToSourceChildrenMap,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.AliasInfo
import Entity.Source qualified as Source
import Entity.VisitInfo
import Path

getVisitEnv :: App (Map.HashMap (Path Abs File) VisitInfo)
getVisitEnv =
  readRef' visitEnv

clearVisitEnv :: App ()
clearVisitEnv =
  writeRef' visitEnv Map.empty

insertToVisitEnv :: Path Abs File -> VisitInfo -> App ()
insertToVisitEnv k v =
  modifyRef' visitEnv $ Map.insert k v

pushToTraceSourceList :: Source.Source -> App ()
pushToTraceSourceList source =
  modifyRef' traceSourceList $ (:) source

popFromTraceSourceList :: App ()
popFromTraceSourceList =
  modifyRef' traceSourceList tail

getTraceSourceList :: App [Source.Source]
getTraceSourceList =
  readRef' traceSourceList

getSourceChildrenMap :: App (Map.HashMap (Path Abs File) [(Source.Source, AliasInfo)])
getSourceChildrenMap =
  readRef' sourceChildrenMap

insertToSourceChildrenMap :: Path Abs File -> [(Source.Source, AliasInfo)] -> App ()
insertToSourceChildrenMap k v =
  modifyRef' sourceChildrenMap $ Map.insert k v
