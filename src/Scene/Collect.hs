module Scene.Collect
  ( collectTargetList,
    collectModuleFiles,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Data.Maybe
import Entity.Module
import Entity.Target
import Path
import Prelude hiding (log)

collectTargetList :: Maybe ConcreteTarget -> App [ConcreteTarget]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Module.getMainModule

collectModuleFiles :: App [FilePath]
collectModuleFiles = do
  mainModule <- Module.getMainModule
  let moduleRootDir = parent $ moduleLocation mainModule
  let tarRootDir = parent moduleRootDir
  relModuleSourceDir <- getRelFilePath tarRootDir $ getSourceDir mainModule
  relModuleFile <- getRelFilePath tarRootDir $ moduleLocation mainModule
  foreignContents <- mapM (getRelFilePath tarRootDir) $ getForeignContents mainModule
  extraContents <- mapM (arrangeExtraContentPath tarRootDir) $ getExtraContents mainModule
  return $ relModuleFile : relModuleSourceDir : foreignContents ++ extraContents

getRelFilePath :: Path Abs Dir -> Path Abs a -> App FilePath
getRelFilePath baseDir path =
  toFilePath <$> Path.stripPrefix baseDir path

arrangeExtraContentPath :: Path Abs Dir -> SomePath Abs -> App FilePath
arrangeExtraContentPath baseDir somePath =
  case somePath of
    Left dirPath ->
      getRelFilePath baseDir dirPath
    Right filePath ->
      getRelFilePath baseDir filePath
