module Command.Archive.Move.Internal.Module.GetExistingVersions (getExistingVersions) where

import Error.Rule.EIO (EIO)
import Command.Archive.Rule.PackageVersion qualified as PV
import Data.List qualified as List
import Data.Maybe
import Kernel.Common.Move.Handle.Global.Path (getBaseName)
import Kernel.Common.Rule.Module
import Path.IO

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
