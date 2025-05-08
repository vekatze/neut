module Command.Archive.Move.Internal.PackageVersion.Reflect (reflect) where

import Command.Archive.Move.Internal.Module.GetExistingVersions
import Command.Archive.Rule.PackageVersion qualified as PV
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Kernel.Common.Rule.Module
import Library.Error.Move.Run (raiseError')
import Library.Error.Rule.EIO (EIO)
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
