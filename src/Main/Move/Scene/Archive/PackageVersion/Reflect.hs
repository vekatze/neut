module Main.Move.Scene.Archive.PackageVersion.Reflect (reflect) where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Main.Move.Context.EIO (EIO, raiseError')
import Main.Move.Scene.Archive.Module.GetExistingVersions
import Main.Rule.Module
import Main.Rule.PackageVersion qualified as PV
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
