module Case.Main.Module
  ( getModuleFilePath,
    getModule,
    getSourcePath,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
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
import qualified Scene.Parse.Core as Parse (Context)

class
  ( Throw.Context m,
    Path.Context m,
    Env.Context m,
    Parse.Context m,
    MonadIO m
  ) =>
  Context m

getModuleFilePath :: Context m => Maybe H.Hint -> MID.ModuleID -> m (Path Abs File)
getModuleFilePath mHint moduleID = do
  moduleDir <- getModuleDirByID mHint moduleID
  return $ moduleDir </> moduleFile

getModule ::
  Context m =>
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  m Module
getModule m moduleID locatorText = do
  nextModuleFilePath <- getModuleFilePath (Just m) moduleID
  moduleCacheMap <- Env.getModuleCacheMap
  case Map.lookup nextModuleFilePath moduleCacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        Throw.raiseError m $
          T.pack "could not find the module file for `"
            <> locatorText
            <> "`"
      nextModule <- Module.fromFilePath moduleID nextModuleFilePath
      Env.insertToModuleCacheMap nextModuleFilePath nextModule
      return nextModule

getSourcePath ::
  Context m =>
  SGL.StrictGlobalLocator ->
  m (Path Abs File)
getSourcePath sgl = do
  moduleDir <- getModuleDirByID Nothing $ SGL.moduleID sgl
  let relPath = SL.reify $ SGL.sourceLocator sgl
  return $ moduleDir </> sourceRelDir </> relPath

getModuleDirByID ::
  Context m =>
  Maybe H.Hint ->
  MID.ModuleID ->
  m (Path Abs Dir)
getModuleDirByID mHint moduleID = do
  mainModule <- Env.getMainModule
  case moduleID of
    MID.Base -> do
      let message = "the base module can't be used here"
      case mHint of
        Just hint ->
          Throw.raiseError hint message
        Nothing ->
          Throw.raiseError' message
    MID.Main ->
      return $ getModuleRootDir mainModule
    MID.Library (MC.ModuleChecksum checksum) -> do
      libraryDir <- Path.getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum
