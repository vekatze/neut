module Act.Release
  ( release,
    Config (..),
    Context,
  )
where

import Entity.Config.Release
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize

class
  ( Initialize.Context m,
    Archive.Context m,
    Collect.Context m
  ) =>
  Context m

release :: Context m => Config -> m ()
release cfg = do
  Initialize.initializeCompiler (logCfg cfg) True
  files <- Collect.collectModuleFiles
  Archive.archive (getReleaseName cfg) files
