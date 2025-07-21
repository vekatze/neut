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
import Gensym.Handle qualified as Gensym
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Module
import Kernel.Common.Module.FromPath qualified as ModuleReflect
import Language.Common.ModuleAlias (ModuleAlias)
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H
import Path.IO

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    moduleHandle :: Module.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

getModule ::
  Handle ->
  MainModule ->
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  App Module
getModule h mainModule m moduleID locatorText = do
  nextModuleFilePath <- Module.getModuleFilePath mainModule (Just m) moduleID
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
      nextModule <- ModuleReflect.fromFilePath nextModuleFilePath
      liftIO $ Module.insertToModuleCacheMap (moduleHandle h) nextModuleFilePath nextModule
      return nextModule

getAllDependencies :: Handle -> MainModule -> Module -> App [(ModuleAlias, Module)]
getAllDependencies h mainModule baseModule = do
  forM (Map.toList $ moduleDependency baseModule) $ \(alias, dependency) -> do
    let moduleID = MID.Library $ dependencyDigest dependency
    moduleFilePath <- Module.getModuleFilePath mainModule Nothing moduleID
    let m = H.newSourceHint moduleFilePath
    dep <- getModule h mainModule m moduleID (MID.reify moduleID)
    return (alias, dep)
