module Move.Scene.Archive.PackageVersion.Reflect (reflect) where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Data.Maybe
import Data.Text qualified as T
import Move.Context.EIO (EIO)
import Move.Scene.Module.GetExistingVersions
import Rule.Error (newError')
import Rule.Module
import Rule.PackageVersion qualified as PV
import Prelude hiding (log)

reflect :: MainModule -> T.Text -> EIO PV.PackageVersion
reflect mainModule versionText = do
  case PV.reflect versionText of
    Nothing ->
      throwError $ newError' "The version must be something like X-Y-Z"
    Just packageVersion -> do
      ensureNewVersionSanity mainModule packageVersion
      return packageVersion

ensureNewVersionSanity :: MainModule -> PV.PackageVersion -> EIO ()
ensureNewVersionSanity targetModule newVersion = do
  existingVersions <- getExistingVersions targetModule
  unless (PV.isValidNewVersion newVersion existingVersions) $
    throwError $
      newError' "A new version must be the newest one in a major release"
