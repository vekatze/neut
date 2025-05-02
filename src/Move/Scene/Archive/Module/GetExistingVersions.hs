module Move.Scene.Archive.Module.GetExistingVersions (getExistingVersions) where

import Data.List qualified as List
import Data.Maybe
import Move.Context.EIO (EIO)
import Move.Context.Path (getBaseName)
import Path.IO
import Rule.Module
import Rule.PackageVersion qualified as PV

getExistingVersions :: MainModule -> EIO [PV.PackageVersion]
getExistingVersions (MainModule targetModule) = do
  let archiveDir = getArchiveDir targetModule
  b <- doesDirExist archiveDir
  if not b
    then return []
    else do
      (_, archiveFiles) <- listDir archiveDir
      basenameList <- mapM getBaseName archiveFiles
      return $ List.sort $ mapMaybe PV.reflect basenameList
