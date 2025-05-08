module Command.Archive.Move.Internal.PackageVersion.ChooseNewVersion
  ( Handle,
    new,
    chooseNewVersion,
  )
where

import Aux.Error.Rule.EIO (EIO)
import Aux.Logger.Move.Log qualified as Logger
import Aux.Logger.Rule.Handle qualified as Logger
import Command.Archive.Move.Internal.Module.GetExistingVersions
import Command.Archive.Rule.PackageVersion qualified as PV
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.Rule.Module (MainModule)
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
