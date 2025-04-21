module Context.Unravel
  ( getVisitEnv,
    initialize,
    insertToVisitEnv,
    pushToTraceSourceList,
    popFromTraceSourceList,
    getTraceSourceList,
    getSourceChildrenMap,
    insertToSourceChildrenMap,
  )
where

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Rule.Import
import Rule.Source qualified as Source
import Rule.VisitInfo
import Path

getVisitEnv :: App (Map.HashMap (Path Abs File) VisitInfo)
getVisitEnv =
  readRef' visitEnv

initialize :: App ()
initialize = do
  writeRef' visitEnv Map.empty
  writeRef' sourceChildrenMap Map.empty
  Antecedent.initialize

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

getSourceChildrenMap :: App (Map.HashMap (Path Abs File) [ImportItem])
getSourceChildrenMap =
  readRef' sourceChildrenMap

insertToSourceChildrenMap :: Path Abs File -> [ImportItem] -> App ()
insertToSourceChildrenMap k v =
  modifyRef' sourceChildrenMap $ Map.insert k v
