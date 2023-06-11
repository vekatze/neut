module Scene.Module.UpdateAntecedents (updateAntecedents) where

import Context.App
import Context.Fetch (getHandleContents)
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.Text qualified as T
import Entity.Const
import Entity.Module
import Entity.ModuleDigest (ModuleDigest)
import Entity.ModuleDigest qualified as MD
import Entity.PackageVersion qualified as PV
import Path
import Scene.Module.GetExistingVersions
import System.IO
import Prelude hiding (log)

updateAntecedents :: PV.PackageVersion -> Module -> App ()
updateAntecedents newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- ListUtils.nubOrd <$> mapM (getDigest targetModule) antecedents
  Module.save $ targetModule {moduleAntecedents = antecedentList}

getPackagePath :: Module -> PV.PackageVersion -> App (Path Abs File)
getPackagePath targetModule ver = do
  let releaseDir = getReleaseDir targetModule
  let releaseName = PV.reify ver
  Path.resolveFile releaseDir $ T.unpack $ releaseName <> packageFileExtension

getDigest :: Module -> PV.PackageVersion -> App ModuleDigest
getDigest targetModule ver = do
  path <- getPackagePath targetModule ver
  handle <- liftIO $ openFile (toFilePath path) ReadMode
  package <- getHandleContents handle
  return $ MD.fromByteString package
