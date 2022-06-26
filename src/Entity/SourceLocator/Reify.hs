module Entity.SourceLocator.Reify (toAbsPath) where

import Control.Exception.Safe
import Entity.Module
import Entity.SourceLocator
import Path

toAbsPath :: MonadThrow m => SourceLocator -> m (Path Abs File)
toAbsPath (SourceLocator baseModule dirNameList fileName) = do
  relPathToSource <- getRelPathToSource dirNameList fileName
  return $ getSourceDir baseModule </> relPathToSource

getRelPathToSource :: MonadThrow m => [DirName] -> FileName -> m (Path Rel File)
getRelPathToSource dirNameList f@(FileName fileName) =
  case dirNameList of
    [] ->
      parseRelFile fileName
    DirName dirName : rest -> do
      relPath <- getRelPathToSource rest f
      dirPath <- parseRelDir dirName
      return $ dirPath </> relPath
