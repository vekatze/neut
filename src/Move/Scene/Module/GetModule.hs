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
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Move.Context.App (App)
import Move.Context.EIO (EIO)
import Move.Context.Module qualified as Module
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path.IO
import Rule.Error (newError)
import Rule.Hint qualified as H
import Rule.Module
import Rule.ModuleAlias (ModuleAlias)
import Rule.ModuleID qualified as MID

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    moduleHandle :: Module.Handle
  }

new :: Gensym.Handle -> Module.Handle -> App Handle
new gensymHandle moduleHandle = do
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
  cacheMap <- liftIO $ Module.getModuleCacheMap (moduleHandle h)
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
      let h' = ModuleReflect.Handle {gensymHandle = gensymHandle h}
      nextModule <- ModuleReflect.fromFilePath h' nextModuleFilePath
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
