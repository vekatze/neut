module Scene.Collect
  ( collectTargetList,
    collectSourceList,
    collectModuleFiles,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Path.IO
import Prelude hiding (log)

collectTargetList :: Maybe Target -> App [Target]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Module.getMainModule

collectSourceList :: Maybe FilePath -> App [SGL.StrictGlobalLocator]
collectSourceList mFilePathStr = do
  mainModule <- Module.getMainModule
  case mFilePathStr of
    Just filePathStr -> do
      path <- parseSourcePathRelativeToSourceDir filePathStr
      return
        [ SGL.StrictGlobalLocator
            { moduleID = MID.Main,
              sourceLocator = SL.SourceLocator path
            }
        ]
    Nothing -> do
      return (Map.elems $ moduleTarget mainModule)

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

parseSourcePathRelativeToSourceDir :: FilePath -> App (Path Rel File)
parseSourcePathRelativeToSourceDir filePathStr = do
  path <- resolveFile' filePathStr
  mainModule <- Module.getMainModule
  let sourceDir = getSourceDir mainModule
  catch (stripProperPrefix sourceDir path) $ \stripProperPrefixException ->
    case stripProperPrefixException of
      NotAProperPrefix _ _ ->
        Throw.raiseError' "specified file isn't a part of current package"
      _ ->
        throwM stripProperPrefixException
