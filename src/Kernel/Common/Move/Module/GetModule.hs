module Kernel.Common.Move.Module.GetModule
  ( Handle (..),
    new,
    getModule,
    getAllDependencies,
  )
where

import Error.Move.Run (raiseError)
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Logger.Rule.Hint qualified as H
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Module qualified as Module
import Kernel.Common.Move.Module.FromPath qualified as ModuleReflect
import Kernel.Common.Rule.Handle.Global.Module qualified as Module
import Kernel.Common.Rule.Module
import Language.Common.Rule.ModuleAlias (ModuleAlias)
import Language.Common.Rule.ModuleID qualified as MID
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
  EIO Module
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

getAllDependencies :: Handle -> MainModule -> Module -> EIO [(ModuleAlias, Module)]
getAllDependencies h mainModule baseModule = do
  forM (Map.toList $ moduleDependency baseModule) $ \(alias, dependency) -> do
    let moduleID = MID.Library $ dependencyDigest dependency
    moduleFilePath <- Module.getModuleFilePath mainModule Nothing moduleID
    let m = H.newSourceHint moduleFilePath
    dep <- getModule h mainModule m moduleID (MID.reify moduleID)
    return (alias, dep)
