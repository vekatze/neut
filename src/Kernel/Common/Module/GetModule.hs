module Kernel.Common.Module.GetModule
  ( Handle (..),
    new,
    getModule,
    getAllDependencies,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Module
import Language.Common.ModuleAlias (ModuleAlias)
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H
import Path.IO

newtype Handle = Handle
  { moduleHandle :: Module.Handle
  }

new :: Module.Handle -> Handle
new moduleHandle = do
  Handle {..}

getModule ::
  Handle ->
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  App Module
getModule h m moduleID locatorText = do
  nextModuleFilePath <- Module.getModuleFilePath (moduleHandle h) (Just m) moduleID
  cacheMap <- liftIO $ Module.getModuleCacheMap (moduleHandle h)
  case Map.lookup nextModuleFilePath cacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        raiseError m $
          T.pack "Could not find the module file for `"
            <> locatorText
            <> "`"
      nextModule <- Module.readModule (moduleHandle h) nextModuleFilePath
      liftIO $ Module.insertToModuleCacheMap (moduleHandle h) nextModuleFilePath nextModule
      return nextModule

getAllDependencies :: Handle -> Module -> App [(ModuleAlias, Module)]
getAllDependencies h baseModule = do
  forM (Map.toList $ moduleDependency baseModule) $ \(alias, dependency) -> do
    let moduleID = MID.Library $ dependencyDigest dependency
    moduleFilePath <- Module.getModuleFilePath (moduleHandle h) Nothing moduleID
    let m = H.newSourceHint moduleFilePath
    dep <- getModule h m moduleID (MID.reify moduleID)
    return (alias, dep)
