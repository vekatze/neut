module Scene.PackageVersion.Reflect (reflect) where

import Context.App
import Context.Module qualified as Module
import Context.Path (getBaseName)
import Context.Throw qualified as Throw
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text qualified as T
import Entity.Module
import Entity.PackageVersion qualified as PV
import Path.IO
import Prelude hiding (log)

reflect :: T.Text -> App PV.PackageVersion
reflect versionText = do
  case PV.reflect versionText of
    Nothing ->
      Throw.raiseError' "the version must be something like X.Y.Z"
    Just packageVersion -> do
      mainModule <- Module.getMainModule
      ensureNewVersionSanity mainModule packageVersion
      return packageVersion

ensureNewVersionSanity :: Module -> PV.PackageVersion -> App ()
ensureNewVersionSanity targetModule newVersion = do
  existingVersions <- getExistingVersions targetModule
  unless (PV.isValidNewVersion newVersion existingVersions) $
    Throw.raiseError' "a new version must be the newest one in a major release"

getExistingVersions :: Module -> App [PV.PackageVersion]
getExistingVersions targetModule = do
  let releaseDir = getReleaseDir targetModule
  (_, releaseFiles) <- listDir releaseDir
  basenameList <- mapM getBaseName releaseFiles
  return $ sort $ mapMaybe PV.reflect basenameList
