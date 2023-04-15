module Scene.Module.UpdateAntecedents (updateAntecedents) where

import Context.App
import Context.Fetch (getHandleContents)
import Context.Module qualified as Module
import Context.Path (getBaseName)
import Context.Path qualified as Path
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Text qualified as T
import Entity.Const
import Entity.Module
import Entity.ModuleChecksum (ModuleChecksum)
import Entity.ModuleChecksum qualified as MC
import Entity.PackageVersion qualified as PV
import Path
import Path.IO
import System.IO
import Prelude hiding (log)

updateAntecedents :: PV.PackageVersion -> Module -> App ()
updateAntecedents newVersion targetModule = do
  existingVersions <- getExistingVersions targetModule
  let antecedents = PV.getAntecedents newVersion existingVersions
  antecedentList <- nub <$> mapM (getChecksum targetModule) antecedents
  Module.save $ targetModule {moduleAntecedents = antecedentList}

getExistingVersions :: Module -> App [PV.PackageVersion]
getExistingVersions targetModule = do
  let releaseDir = getReleaseDir targetModule
  (_, releaseFiles) <- listDir releaseDir
  basenameList <- mapM getBaseName releaseFiles
  return $ sort $ mapMaybe PV.reflect basenameList

getPackagePath :: Module -> PV.PackageVersion -> App (Path Abs File)
getPackagePath targetModule ver = do
  let releaseDir = getReleaseDir targetModule
  let releaseName = PV.reify ver
  Path.resolveFile releaseDir $ T.unpack $ releaseName <> packageFileExtension

getChecksum :: Module -> PV.PackageVersion -> App ModuleChecksum
getChecksum targetModule ver = do
  path <- getPackagePath targetModule ver
  handle <- liftIO $ openFile (toFilePath path) ReadMode
  package <- getHandleContents handle
  return $ MC.fromByteString package
