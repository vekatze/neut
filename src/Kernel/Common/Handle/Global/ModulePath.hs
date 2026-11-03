module Kernel.Common.Handle.Global.ModulePath
  ( Handle (..),
    ModulePath,
    ModulePathMap,
    new,
    get,
    set,
    build,
    renderDD,
    renderCanonicalDD,
    renderSource,
    renderCanonicalSource,
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
import Kernel.Common.Module.GetDependency qualified as GetDependency
import Kernel.Common.Source qualified as Source
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
  let getDependencyHandle = GetDependency.new (_moduleHandle h) (_antecedentHandle h)
  build' getDependencyHandle initialMap [(M.extractModule (_mainModule h), [])]

build' ::
  GetDependency.Handle ->
  ModulePathMap ->
  [(M.Module, ModulePath)] ->
  App ModulePathMap
build' getDependencyHandle modulePathMap queue = do
  case queue of
    [] ->
      return modulePathMap
    (currentModule, currentPath) : rest -> do
      (modulePathMap', newQueueItems) <-
        foldM
          (registerDependency getDependencyHandle currentModule currentPath)
          (modulePathMap, [])
          (getSortedDependencyList currentModule)
      build' getDependencyHandle modulePathMap' (rest <> reverse newQueueItems)

registerDependency ::
  GetDependency.Handle ->
  M.Module ->
  ModulePath ->
  (ModulePathMap, [(M.Module, ModulePath)]) ->
  (MA.ModuleAlias, M.Dependency) ->
  App (ModulePathMap, [(M.Module, ModulePath)])
registerDependency getDependencyHandle currentModule path (modulePathMap, queue) (alias, dependency) = do
  let m = H.newSourceHint $ M.moduleLocation currentModule
  childModule <- GetDependency.getDependency getDependencyHandle m alias dependency
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

renderDD :: ModulePathMap -> DD.DefiniteDescription -> T.Text
renderDD =
  renderDDWith True

renderCanonicalDD :: ModulePathMap -> DD.DefiniteDescription -> T.Text
renderCanonicalDD =
  renderDDWith False

renderDDWith :: Bool -> ModulePathMap -> DD.DefiniteDescription -> T.Text
renderDDWith abbreviate modulePathMap dd = do
  let (moduleID, locator) = DD.unconsDD dd
  case Map.lookup moduleID modulePathMap of
    Just []
      | abbreviate ->
          DD.localLocator dd
      | otherwise ->
          MA.reify MA.thisModuleAlias <> doubleColon <> locator
    Just modulePath ->
      T.intercalate nsSep modulePath <> doubleColon <> locator
    Nothing ->
      DD.reify dd

renderSource :: ModulePathMap -> Source.Source -> IO T.Text
renderSource modulePathMap source = do
  renderSourceWith True modulePathMap source

renderCanonicalSource :: ModulePathMap -> Source.Source -> IO T.Text
renderCanonicalSource modulePathMap source = do
  renderSourceWith False modulePathMap source

renderSourceWith :: Bool -> ModulePathMap -> Source.Source -> IO T.Text
renderSourceWith abbreviate modulePathMap source = do
  let sourceModule = Source.sourceModule source
  let moduleID = M.moduleID sourceModule
  let modulePath = Map.lookupDefault [MID.reify moduleID] moduleID modulePathMap
  locator <- Source.getBaseReadableLocator source
  case (abbreviate, modulePath) of
    (True, []) ->
      return locator
    (_, []) ->
      return $ MA.reify MA.thisModuleAlias <> doubleColon <> locator
    (_, _) ->
      return $ T.intercalate nsSep modulePath <> doubleColon <> locator
