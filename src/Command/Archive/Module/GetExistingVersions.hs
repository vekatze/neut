module Command.Archive.Module.GetExistingVersions (getExistingVersions) where

import App.App (App)
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Data.List qualified as List
import Data.Maybe
import Kernel.Common.Handle.Global.Path (getBaseName)
import Kernel.Common.Module
import Path.IO

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
