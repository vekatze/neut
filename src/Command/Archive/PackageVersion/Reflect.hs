module Command.Archive.PackageVersion.Reflect (reflect) where

import Command.Archive.Module.GetExistingVersions
import Command.Archive.PackageVersion.PackageVersion qualified as PV
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError')
import Kernel.Common.Module
import Prelude hiding (log)

reflect :: MainModule -> T.Text -> EIO PV.PackageVersion
reflect mainModule versionText = do
  case PV.reflect versionText of
    Nothing ->
      raiseError' "The version must be something like X-Y-Z"
    Just packageVersion -> do
      ensureNewVersionSanity mainModule packageVersion
      return packageVersion

ensureNewVersionSanity :: MainModule -> PV.PackageVersion -> EIO ()
ensureNewVersionSanity targetModule newVersion = do
  existingVersions <- getExistingVersions targetModule
  unless (PV.isValidNewVersion newVersion existingVersions) $
    raiseError' "A new version must be the newest one in a major release"
