module Scene.PackageVersion.ChooseNewVersion (chooseNewVersion) where

import Context.App
import Context.Module qualified as Module
import Context.Remark (printNote')
import Control.Monad
import Entity.PackageVersion qualified as PV
import Scene.Module.GetExistingVersions
import Prelude hiding (log)

chooseNewVersion :: App PV.PackageVersion
chooseNewVersion = do
  mainModule <- Module.getMainModule
  existingVersions <- getExistingVersions mainModule
  newVersion <-
    case existingVersions of
      [] -> do
        return PV.initialVersion
      v : vs ->
        return $ PV.increment $ PV.getNewestVersion vs v
  printNote' $ "Selected a new version: " <> PV.reify newVersion
  return newVersion
