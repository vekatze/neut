module Move.Scene.Module.GetExistingVersions (getExistingVersions) where

import Data.List qualified as List
import Data.Maybe
import Move.Context.App
import Move.Context.Path (getBaseName)
import Path.IO
import Rule.Module
import Rule.PackageVersion qualified as PV

getExistingVersions :: MainModule -> App [PV.PackageVersion]
getExistingVersions (MainModule targetModule) = do
  let archiveDir = getArchiveDir targetModule
  b <- doesDirExist archiveDir
  if not b
    then return []
    else do
      (_, archiveFiles) <- listDir archiveDir
      basenameList <- mapM getBaseName archiveFiles
      return $ List.sort $ mapMaybe PV.reflect basenameList
