module Main.Move.Scene.Archive.Module.GetExistingVersions (getExistingVersions) where

import Data.List qualified as List
import Data.Maybe
import Error.Rule.EIO (EIO)
import Main.Move.Context.Path (getBaseName)
import Main.Rule.Module
import Main.Rule.PackageVersion qualified as PV
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
