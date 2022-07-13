module Context.Module.Main (new) where

import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Const
import qualified Entity.Hint as H
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Entity.ModuleChecksum as MC
import qualified Entity.ModuleID as MID
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Path
import Path.IO

new :: Module.Config -> IO Module.Context
new cfg = do
  moduleCacheMapRef <- newIORef Map.empty
  return $
    Module.Context
      { Module.getModuleFilePath =
          getModuleFilePath
            (Module.throwCtx cfg)
            (Module.pathCtx cfg)
            (Module.mainModule cfg),
        Module.getModule =
          getModule
            (Module.throwCtx cfg)
            (Module.pathCtx cfg)
            (Module.mainModule cfg)
            moduleCacheMapRef,
        Module.getSourcePath =
          getSourcePath
            (Module.throwCtx cfg)
            (Module.pathCtx cfg)
            (Module.mainModule cfg)
      }

getModuleFilePath :: Throw.Context -> Path.Context -> Module -> Maybe H.Hint -> MID.ModuleID -> IO (Path Abs File)
getModuleFilePath throwCtx pathCtx mainModule mHint moduleID = do
  moduleDir <- getModuleDirByID throwCtx pathCtx mainModule mHint moduleID
  return $ moduleDir </> moduleFile

getModule ::
  Throw.Context ->
  Path.Context ->
  Module ->
  IORef (Map.HashMap (Path Abs File) Module) ->
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  IO Module
getModule throwCtx pathCtx mainModule moduleCacheMapRef m moduleID locatorText = do
  nextModuleFilePath <- getModuleFilePath throwCtx pathCtx mainModule (Just m) moduleID
  moduleCacheMap <- readIORef moduleCacheMapRef
  case Map.lookup nextModuleFilePath moduleCacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        Throw.raiseError throwCtx m $
          T.pack "could not find the module file for `"
            <> locatorText
            <> "`"
      nextModule <- Module.fromFilePath throwCtx moduleID nextModuleFilePath
      modifyIORef' moduleCacheMapRef $ Map.insert nextModuleFilePath nextModule
      return nextModule

getSourcePath ::
  Throw.Context ->
  Path.Context ->
  Module ->
  SGL.StrictGlobalLocator ->
  IO (Path Abs File)
getSourcePath throwCtx pathCtx mainModule sgl = do
  moduleDir <- getModuleDirByID throwCtx pathCtx mainModule Nothing $ SGL.moduleID sgl
  let relPath = SL.reify $ SGL.sourceLocator sgl
  return $ moduleDir </> sourceRelDir </> relPath

getModuleDirByID ::
  Throw.Context ->
  Path.Context ->
  Module ->
  Maybe H.Hint ->
  MID.ModuleID ->
  IO (Path Abs Dir)
getModuleDirByID throwCtx pathCtx mainModule mHint moduleID = do
  case moduleID of
    MID.Base -> do
      let message = "the base module can't be used here"
      case mHint of
        Just hint ->
          Throw.raiseError throwCtx hint message
        Nothing ->
          Throw.raiseError' throwCtx message
    MID.Main ->
      return $ getModuleRootDir mainModule
    MID.Library (MC.ModuleChecksum checksum) -> do
      libraryDir <- Path.getLibraryDirPath pathCtx
      resolveDir libraryDir $ T.unpack checksum
