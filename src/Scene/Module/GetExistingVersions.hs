module Scene.Module.GetExistingVersions (getExistingVersions) where

import Context.App
import Context.Path (getBaseName)
import Data.List
import Data.Maybe
import Entity.Module
import Entity.PackageVersion qualified as PV
import Path.IO

getExistingVersions :: Module -> App [PV.PackageVersion]
getExistingVersions targetModule = do
  let releaseDir = getReleaseDir targetModule
  b <- doesDirExist releaseDir
  if not b
    then return []
    else do
      (_, releaseFiles) <- listDir releaseDir
      basenameList <- mapM getBaseName releaseFiles
      return $ sort $ mapMaybe PV.reflect basenameList
