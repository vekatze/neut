module Move.Scene.PackageVersion.ChooseNewVersion
  ( Handle,
    new,
    chooseNewVersion,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Scene.Module.GetExistingVersions
import Rule.Module (MainModule)
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

newtype Handle
  = Handle
  { reportHandle :: Report.Handle
  }

new :: App Handle
new = do
  reportHandle <- Report.new
  return $ Handle {..}

chooseNewVersion :: Handle -> MainModule -> EIO PV.PackageVersion
chooseNewVersion h mainModule = do
  existingVersions <- getExistingVersions mainModule
  newVersion <-
    case existingVersions of
      [] -> do
        return PV.initialVersion
      v : vs ->
        return $ PV.increment $ PV.getNewestVersion vs v
  liftIO $ Report.printNote' (reportHandle h) $ "Selected a new version: " <> PV.reify newVersion
  return newVersion
