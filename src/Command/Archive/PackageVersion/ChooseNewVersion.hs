module Command.Archive.PackageVersion.ChooseNewVersion (chooseNewVersion) where

import App.App (App)
import Command.Archive.Module.GetExistingVersions
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Control.Monad
import Kernel.Common.Module (MainModule)
import Prelude hiding (log)

chooseNewVersion :: MainModule -> App PV.PackageVersion
chooseNewVersion mainModule = do
  existingVersions <- getExistingVersions mainModule
  case existingVersions of
    [] -> do
      return PV.initialVersion
    v : vs ->
      return $ PV.increment $ PV.getNewestVersion vs v
