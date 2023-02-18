module Scene.Collect
  ( collectTargetList,
    collectSourceList,
    collectModuleFiles,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Entity.Module
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Prelude hiding (log)

collectTargetList :: Maybe Target -> App [Target]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Module.getMainModule

collectSourceList :: Maybe FilePath -> App [SGL.StrictGlobalLocator]
collectSourceList mFilePathStr = do
  mainModule <- Module.getMainModule
  case mFilePathStr of
    Just filePathStr -> do
      sgl <- SGL.reflectInMainModule filePathStr
      return [sgl]
    Nothing -> do
      return (Map.elems $ moduleTarget mainModule)

collectModuleFiles :: App [FilePath]
collectModuleFiles = do
  mainModule <- Module.getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  let tarRootDir = parent moduleRootDir
  relModuleSourceDir <- Path.stripPrefix tarRootDir $ getSourceDir mainModule
  relModuleFile <- Path.stripPrefix tarRootDir $ moduleLocation mainModule
  extraContents <- mapM (arrangeExtraContentPath tarRootDir) $ moduleExtraContents mainModule
  return $ toFilePath relModuleFile : toFilePath relModuleSourceDir : extraContents

arrangeExtraContentPath :: Path Abs Dir -> SomePath -> App FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> Path.stripPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> Path.stripPrefix tarRootDir filePath
