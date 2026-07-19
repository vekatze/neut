module Kernel.Parse.Internal.Handle.Alias
  ( Handle,
    new,
    resolveAlias,
    getModuleLocation,
    resolveModuleAlias,
    activateImportUse,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.Import (ImportUse (..))
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
import Path (Abs, File, Path)

data Handle = Handle
  { shiftToLatestHandle :: STL.Handle,
    locatorHandle :: Locator.Handle,
    envHandle :: Env.Handle,
    moduleHandle :: ModuleHandle.Handle,
    currentModule :: Module,
    modulePathCacheRef :: IORef (Map.HashMap [ModuleAlias] MID.ModuleID)
  }

new :: STL.Handle -> Locator.Handle -> Env.Handle -> ModuleHandle.Handle -> Source.Source -> IO Handle
new shiftToLatestHandle locatorHandle envHandle moduleHandle source = do
  let currentModule = Source.sourceModule source
  modulePathCacheRef <- newIORef Map.empty
  return $ Handle {..}

resolveAlias ::
  Handle ->
  Hint ->
  GL.GlobalLocator ->
  App SGL.StrictGlobalLocator
resolveAlias h m gl = do
  case gl of
    GL.GlobalLocator {modulePath, sourceLocator} -> do
      moduleID <- resolveModulePath h m modulePath
      return $ SGL.new moduleID sourceLocator

resolveModulePath :: Handle -> Hint -> [ModuleAlias] -> App MID.ModuleID
resolveModulePath h m modulePath = do
  cache <- liftIO $ readIORef (modulePathCacheRef h)
  case Map.lookup modulePath cache of
    Just cachedModuleID ->
      return cachedModuleID
    Nothing -> do
      result <- resolveModulePathFrom h m (currentModule h) 0 modulePath
      liftIO $ modifyIORef' (modulePathCacheRef h) $ Map.insert modulePath result
      return result

resolveModulePathFrom :: Handle -> Hint -> Module -> Int -> [ModuleAlias] -> App MID.ModuleID
resolveModulePathFrom h m baseModule depth modulePath =
  case modulePath of
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
          resolveModulePathFrom h m nextModule (depth + 1) rest

getModuleLocation :: Handle -> Hint -> MID.ModuleID -> App (Path Abs File)
getModuleLocation h m moduleID = do
  case moduleID of
    MID.Base ->
      return $ moduleLocation $ extractModule $ Env.getMainModule $ envHandle h
    _ ->
      ModuleHandle.getModuleFilePath (Env.getMainModule $ envHandle h) (Just m) moduleID

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
  resolveModulePath h m [moduleAlias]

activateImportUse :: Handle -> Source.Source -> TopNameMap -> ImportUse -> App ()
activateImportUse h source topNameMap importUse =
  case importUse of
    ImportUse shouldUpdateTag strictGlobalLocator entries ->
      Locator.activateImportedEntries (locatorHandle h) source topNameMap shouldUpdateTag strictGlobalLocator entries
