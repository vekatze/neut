module Move.Scene.Archive.PackageVersion.ChooseNewVersion
  ( Handle,
    new,
    chooseNewVersion,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Logger.Move.Log qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Move.Context.EIO (EIO)
import Move.Scene.Archive.Module.GetExistingVersions
import Rule.Module (MainModule)
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

newtype Handle
  = Handle
  { loggerHandle :: Logger.Handle
  }

new :: Logger.Handle -> Handle
new loggerHandle = do
  Handle {..}

chooseNewVersion :: Handle -> MainModule -> EIO PV.PackageVersion
chooseNewVersion h mainModule = do
  existingVersions <- getExistingVersions mainModule
  newVersion <-
    case existingVersions of
      [] -> do
        return PV.initialVersion
      v : vs ->
        return $ PV.increment $ PV.getNewestVersion vs v
  liftIO $ Logger.printNote' (loggerHandle h) $ "Selected a new version: " <> PV.reify newVersion
  return newVersion
