module Scene.PackageVersion.Reflect (reflect) where

import Context.App
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Rule.Module
import Rule.PackageVersion qualified as PV
import Scene.Module.GetExistingVersions
import Prelude hiding (log)

reflect :: T.Text -> App PV.PackageVersion
reflect versionText = do
  case PV.reflect versionText of
    Nothing ->
      Throw.raiseError' "The version must be something like X-Y-Z"
    Just packageVersion -> do
      mainModule <- Env.getMainModule
      ensureNewVersionSanity mainModule packageVersion
      return packageVersion

ensureNewVersionSanity :: Module -> PV.PackageVersion -> App ()
ensureNewVersionSanity targetModule newVersion = do
  existingVersions <- getExistingVersions targetModule
  unless (PV.isValidNewVersion newVersion existingVersions) $
    Throw.raiseError' "A new version must be the newest one in a major release"
