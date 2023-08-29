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
  relModuleSourceDir <- Path.stripPrefix tarRootDir $ getSourceDir mainModule
  relModuleFile <- Path.stripPrefix tarRootDir $ moduleLocation mainModule
  foreignContents <- mapM (arrangeForeignContentPath tarRootDir) $ moduleForeignDirList mainModule
  extraContents <- mapM (arrangeExtraContentPath tarRootDir) $ moduleExtraContents mainModule
  return $ toFilePath relModuleFile : toFilePath relModuleSourceDir : foreignContents ++ extraContents

arrangeForeignContentPath :: Path Abs Dir -> Path Abs Dir -> App FilePath
arrangeForeignContentPath tarRootDir foreignDir =
  toFilePath <$> Path.stripPrefix tarRootDir foreignDir

arrangeExtraContentPath :: Path Abs Dir -> SomePath -> App FilePath
arrangeExtraContentPath tarRootDir somePath =
  case somePath of
    Left dirPath ->
      toFilePath <$> Path.stripPrefix tarRootDir dirPath
    Right filePath ->
      toFilePath <$> Path.stripPrefix tarRootDir filePath

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
