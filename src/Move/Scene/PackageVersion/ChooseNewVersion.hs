module Move.Scene.PackageVersion.ChooseNewVersion (chooseNewVersion) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report (printNote')
import Move.Context.EIO (EIO)
import Move.Scene.Module.GetExistingVersions
import Rule.Log (ColorSpec)
import Rule.Module (MainModule)
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

chooseNewVersion :: ColorSpec -> MainModule -> EIO PV.PackageVersion
chooseNewVersion c mainModule = do
  existingVersions <- getExistingVersions mainModule
  newVersion <-
    case existingVersions of
      [] -> do
        return PV.initialVersion
      v : vs ->
        return $ PV.increment $ PV.getNewestVersion vs v
  liftIO $ printNote' c $ "Selected a new version: " <> PV.reify newVersion
  return newVersion
