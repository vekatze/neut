module Kernel.Parse.Internal.Handle.Alias
  ( Handle,
    new,
    resolveAlias,
    resolveModuleAlias,
    activateAliasInfo,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.AliasInfo
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as ModuleHandle
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Module
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Common.TopNameMap
import Language.Common.BaseName qualified as BN
import Language.Common.GlobalLocator qualified as GL
import Language.Common.ModuleAlias
import Language.Common.ModuleID qualified as MID
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint

data Handle = Handle
  { shiftToLatestHandle :: STL.Handle,
    locatorHandle :: Locator.Handle,
    envHandle :: Env.Handle,
    moduleHandle :: ModuleHandle.Handle,
    currentModule :: Module,
    moduleRouteCacheRef :: IORef (Map.HashMap [ModuleAlias] MID.ModuleID)
  }

new :: STL.Handle -> Locator.Handle -> Env.Handle -> ModuleHandle.Handle -> Source.Source -> IO Handle
new shiftToLatestHandle locatorHandle envHandle moduleHandle source = do
  let currentModule = Source.sourceModule source
  moduleRouteCacheRef <- newIORef Map.empty
  return $ Handle {..}

resolveAlias ::
  Handle ->
  Hint ->
  GL.GlobalLocator ->
  App SGL.StrictGlobalLocator
resolveAlias h m gl = do
  case gl of
    GL.GlobalLocator {moduleRoute, sourceLocator} -> do
      moduleID <- resolveModuleRoute h m moduleRoute
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }

resolveModuleRoute :: Handle -> Hint -> [ModuleAlias] -> App MID.ModuleID
resolveModuleRoute h m moduleRoute = do
  cache <- liftIO $ readIORef (moduleRouteCacheRef h)
  case Map.lookup moduleRoute cache of
    Just cachedRoute ->
      return cachedRoute
    Nothing -> do
      result <- resolveModuleRouteFrom h m (currentModule h) 0 moduleRoute
      liftIO $ modifyIORef' (moduleRouteCacheRef h) $ Map.insert moduleRoute result
      return result

resolveModuleRouteFrom :: Handle -> Hint -> Module -> Int -> [ModuleAlias] -> App MID.ModuleID
resolveModuleRouteFrom h m baseModule depth route =
  case route of
    [] ->
      return $ moduleID baseModule
    moduleAlias : rest -> do
      when (depth > 0 && isPrivate moduleAlias) $
        raiseError m $
          "No such module alias is defined: " <> BN.reify (extract moduleAlias)
      nextModuleID <- resolveModuleAliasIn h m baseModule moduleAlias
      case rest of
        [] ->
          return nextModuleID
        _ -> do
          nextModule <- getModuleByID h m nextModuleID
          resolveModuleRouteFrom h m nextModule (depth + 1) rest

resolveModuleAliasIn :: Handle -> Hint -> Module -> ModuleAlias -> App MID.ModuleID
resolveModuleAliasIn h m baseModule moduleAlias = do
  let dependencyMap = moduleDependency baseModule
  case Map.lookup moduleAlias dependencyMap of
    Just dep -> do
      let dependencyID = MID.Library $ dependencyDigest dep
      liftIO $ STL.shiftToLatestModuleID (shiftToLatestHandle h) dependencyID
    Nothing
      | moduleAlias == baseModuleAlias ->
          return MID.Base
      | moduleAlias == coreModuleAlias ->
          return $ moduleID baseModule
      | otherwise ->
          raiseError m $
            "No such module alias is defined: " <> BN.reify (extract moduleAlias)

getModuleByID :: Handle -> Hint -> MID.ModuleID -> App Module
getModuleByID h m moduleID = do
  case moduleID of
    MID.Base ->
      raiseError m "The base module cannot be used here"
    _ -> do
      let mainModule = Env.getMainModule (envHandle h)
      let getModuleHandle = GetModule.Handle {GetModule.moduleHandle = moduleHandle h}
      GetModule.getModule getModuleHandle mainModule m moduleID (MID.reify moduleID)

resolveModuleAlias :: Handle -> Hint -> ModuleAlias -> App MID.ModuleID
resolveModuleAlias h m moduleAlias =
  resolveModuleRoute h m [moduleAlias]

activateAliasInfo :: Handle -> Source.Source -> TopNameMap -> AliasInfo -> App ()
activateAliasInfo h source topNameMap aliasInfo =
  case aliasInfo of
    Use shouldUpdateTag strictGlobalLocator localLocatorList ->
      Locator.activateSpecifiedNames (locatorHandle h) source topNameMap shouldUpdateTag strictGlobalLocator localLocatorList
