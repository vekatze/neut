module Context.Case.Main.Module
  ( getModuleFilePath,
    getModule,
    getSourcePath,
    Context,
  )
where

import Context.Env qualified as Env
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Const
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleChecksum qualified as MC
import Entity.ModuleID qualified as MID
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Path
import Path.IO
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as Parse (Context)

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
