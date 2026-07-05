module Kernel.Common.Handle.Global.ModulePath
  ( Handle (..),
    ModulePath,
    ModulePathMap,
    new,
    get,
    set,
    build,
    directModulePathMap,
    renderDD,
    renderSource,
  )
where

import App.App (App)
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.List (sortOn)
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Module qualified as ModuleHandle
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Language.Common.Const
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H

type ModulePath =
  [T.Text]

type ModulePathMap =
  Map.HashMap MID.ModuleID ModulePath

data Handle = Handle
  { _modulePathMapRef :: IORef ModulePathMap,
    _moduleHandle :: ModuleHandle.Handle,
    _antecedentHandle :: Antecedent.Handle,
    _mainModule :: M.MainModule
  }

new :: ModuleHandle.Handle -> Antecedent.Handle -> M.MainModule -> IO Handle
new _moduleHandle _antecedentHandle _mainModule = do
  _modulePathMapRef <- newIORef Map.empty
  return $ Handle {..}

get :: Handle -> IO ModulePathMap
get h =
  readIORef (_modulePathMapRef h)

set :: Handle -> ModulePathMap -> IO ()
set h =
  writeIORef (_modulePathMapRef h)

build :: Handle -> App ModulePathMap
build h = do
  let initialMap = Map.fromList [(MID.Main, []), (MID.Base, ["base"])]
  let shiftToLatestHandle = STL.new (_antecedentHandle h)
  build' (_moduleHandle h) shiftToLatestHandle (_mainModule h) initialMap [(M.extractModule (_mainModule h), [])]

build' ::
  ModuleHandle.Handle ->
  STL.Handle ->
  M.MainModule ->
  ModulePathMap ->
  [(M.Module, ModulePath)] ->
  App ModulePathMap
build' moduleHandle shiftToLatestHandle mainModule modulePathMap queue = do
  case queue of
    [] ->
      return modulePathMap
    (currentModule, currentPath) : rest -> do
      (modulePathMap', newQueueItems) <-
        foldM
          (registerDependency moduleHandle shiftToLatestHandle mainModule currentModule currentPath)
          (modulePathMap, [])
          (getSortedDependencyList currentModule)
      build' moduleHandle shiftToLatestHandle mainModule modulePathMap' (rest <> reverse newQueueItems)

registerDependency ::
  ModuleHandle.Handle ->
  STL.Handle ->
  M.MainModule ->
  M.Module ->
  ModulePath ->
  (ModulePathMap, [(M.Module, ModulePath)]) ->
  (MA.ModuleAlias, M.Dependency) ->
  App (ModulePathMap, [(M.Module, ModulePath)])
registerDependency moduleHandle shiftToLatestHandle mainModule currentModule path (modulePathMap, queue) (alias, dependency) = do
  let moduleID = MID.Library $ M.dependencyDigest dependency
  let m = H.newSourceHint $ M.moduleLocation currentModule
  dependencyModule <- GetModule.getModule (GetModule.Handle {moduleHandle}) mainModule m moduleID (MA.reify alias)
  childModule <- STL.shiftToLatestModule shiftToLatestHandle dependencyModule
  let childModuleID = M.moduleID childModule
  case Map.lookup childModuleID modulePathMap of
    Just _ ->
      return (modulePathMap, queue)
    Nothing -> do
      let childPath = path <> [MA.reify alias]
      let modulePathMap' = Map.insert childModuleID childPath modulePathMap
      return (modulePathMap', (childModule, childPath) : queue)

getSortedDependencyList :: M.Module -> [(MA.ModuleAlias, M.Dependency)]
getSortedDependencyList currentModule = do
  let dependencyList = Map.toList $ M.moduleDependency currentModule
  sortOn (MA.reify . fst) dependencyList

directModulePathMap :: M.Module -> ModulePathMap
directModulePathMap baseModule = do
  let dependencyList = getSortedDependencyList baseModule
  let dependencyPathList =
        flip map dependencyList $ \(alias, dependency) -> do
          (MID.Library (M.dependencyDigest dependency), [MA.reify alias])
  Map.fromList $ [(MID.Main, []), (MID.Base, ["base"])] <> dependencyPathList

renderDD :: ModulePathMap -> DD.DefiniteDescription -> T.Text
renderDD modulePathMap dd = do
  let (moduleID, locator) = DD.unconsDD dd
  case Map.lookup moduleID modulePathMap of
    Just modulePath ->
      renderPathWithLocator modulePath locator
    Nothing ->
      DD.reify dd

renderSource :: ModulePathMap -> Source.Source -> IO T.Text
renderSource modulePathMap source = do
  let sourceModule = Source.sourceModule source
  let moduleID = M.moduleID sourceModule
  let modulePath = Map.lookupDefault [MID.reify moduleID] moduleID modulePathMap
  locator <- Source.getBaseReadableLocator source
  return $ renderLastSegmentWithLocator modulePath locator

renderPathWithLocator :: ModulePath -> T.Text -> T.Text
renderPathWithLocator modulePath locator = do
  if T.null locator
    then renderModulePath modulePath
    else
      if null modulePath
        then locator
        else T.intercalate nsSep modulePath <> routeSep <> locator

renderModulePath :: ModulePath -> T.Text
renderModulePath =
  T.intercalate " -> "

renderLastSegmentWithLocator :: ModulePath -> T.Text -> T.Text
renderLastSegmentWithLocator modulePath locator = do
  case reverse modulePath of
    [] ->
      locator
    lastSegment : _ ->
      lastSegment <> nsSep <> locator
