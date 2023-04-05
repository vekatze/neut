module Act.Release (release) where

import Context.App
import Entity.Config.Release
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize

release :: Config -> App ()
release cfg = do
  Initialize.initializeCompiler (logCfg cfg) Nothing
  files <- Collect.collectModuleFiles
  Archive.archive (getReleaseName cfg) files
