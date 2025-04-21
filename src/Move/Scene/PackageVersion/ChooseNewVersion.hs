module Move.Scene.PackageVersion.ChooseNewVersion (chooseNewVersion) where

import Control.Monad
import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Remark (printNote')
import Move.Scene.Module.GetExistingVersions
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

chooseNewVersion :: App PV.PackageVersion
chooseNewVersion = do
  mainModule <- Env.getMainModule
  existingVersions <- getExistingVersions mainModule
  newVersion <-
    case existingVersions of
      [] -> do
        return PV.initialVersion
      v : vs ->
        return $ PV.increment $ PV.getNewestVersion vs v
  printNote' $ "Selected a new version: " <> PV.reify newVersion
  return newVersion
