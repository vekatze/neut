module Command.Archive.PackageVersion.ChooseNewVersion
  ( Handle,
    new,
    chooseNewVersion,
  )
where

import Command.Archive.Module.GetExistingVersions
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.EIO (EIO)
import Kernel.Common.Module (MainModule)
import Logger.Handle qualified as Logger
import Logger.MoveLog qualified as Logger
import Prelude hiding (log)

newtype Handle = Handle
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
