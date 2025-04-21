module Scene.Module.GetExistingVersions (getExistingVersions) where

import Context.App
import Context.Path (getBaseName)
import Data.List qualified as List
import Data.Maybe
import Rule.Module
import Rule.PackageVersion qualified as PV
import Path.IO

getExistingVersions :: Module -> App [PV.PackageVersion]
getExistingVersions targetModule = do
  let archiveDir = getArchiveDir targetModule
  b <- doesDirExist archiveDir
  if not b
    then return []
    else do
      (_, archiveFiles) <- listDir archiveDir
      basenameList <- mapM getBaseName archiveFiles
      return $ List.sort $ mapMaybe PV.reflect basenameList
