module Move.Scene.Module.GetModule
  ( Handle (..),
    new,
    getModule,
    getAllDependencies,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Unlift (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App (App)
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO)
import Move.Context.Module qualified as Module
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Module.Reflect qualified as Module
import Path
import Path.IO
import Rule.Error (newError)
import Rule.Hint qualified as H
import Rule.Module
import Rule.ModuleAlias (ModuleAlias)
import Rule.ModuleID qualified as MID

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    mcm :: IORef (Map.HashMap (Path Abs File) Module)
  }

new :: App Handle
new = do
  gensymHandle <- Gensym.new
  mcm <- asks App.moduleCacheMap
  return $ Handle {..}

getModule ::
  Handle ->
  MainModule ->
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  EIO Module
getModule h mainModule m moduleID locatorText = do
  nextModuleFilePath <- Module.getModuleFilePath mainModule (Just m) moduleID
  cacheMap <- liftIO $ readIORef (mcm h)
  case Map.lookup nextModuleFilePath cacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        throwError $
          newError m $
            T.pack "Could not find the module file for `"
              <> locatorText
              <> "`"
      let h' = Module.Handle {gensymHandle = gensymHandle h}
      nextModule <- Module.fromFilePath h' nextModuleFilePath
      liftIO $ modifyIORef' (mcm h) $ Map.insert nextModuleFilePath nextModule
      return nextModule

getAllDependencies :: Handle -> MainModule -> Module -> EIO [(ModuleAlias, Module)]
getAllDependencies h mainModule baseModule = do
  forM (Map.toList $ moduleDependency baseModule) $ \(alias, dependency) -> do
    let moduleID = MID.Library $ dependencyDigest dependency
    moduleFilePath <- Module.getModuleFilePath mainModule Nothing moduleID
    let m = H.newSourceHint moduleFilePath
    dep <- getModule h mainModule m moduleID (MID.reify moduleID)
    return (alias, dep)
