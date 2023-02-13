module Scene.Collect
  ( Context,
    collectTargetList,
    collectSourceList,
    collectModuleFiles,
  )
where

import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Entity.Module
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Prelude hiding (log)

class
  ( Env.Context m,
    Path.Context m,
    MonadThrow m
  ) =>
  Context m

collectTargetList :: Context m => Maybe Target -> m [Target]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Env.getMainModule

collectSourceList :: Context m => Maybe FilePath -> m [SGL.StrictGlobalLocator]
collectSourceList mFilePathStr = do
  mainModule <- Env.getMainModule
  case mFilePathStr of
    Just filePathStr -> do
      sgl <- SGL.reflectInMainModule filePathStr
      return [sgl]
    Nothing -> do
      return (Map.elems $ moduleTarget mainModule)

collectModuleFiles :: Context m => m [FilePath]
collectModuleFiles = do
  mainModule <- Env.getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  let tarRootDir = parent moduleRootDir
  relModuleSourceDir <- Path.stripPrefix tarRootDir $ getSourceDir mainModule
  relModuleFile <- Path.stripPrefix tarRootDir $ moduleLocation mainModule
  extraContents <- mapM (arrangeExtraContentPath tarRootDir) $ moduleExtraContents mainModule
  return $ toFilePath relModuleFile : toFilePath relModuleSourceDir : extraContents

arrangeExtraContentPath :: Context m => Path Abs Dir -> SomePath -> m FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> Path.stripPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> Path.stripPrefix tarRootDir filePath
