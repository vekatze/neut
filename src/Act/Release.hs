module Act.Release (release) where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Entity.Config.Release
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize
import Scene.Module.UpdateAntecedents
import Scene.PackageVersion.Reflect qualified as PV

release :: Config -> App ()
release cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Path.ensureNotInLibDir
  packageVersion <- PV.reflect (getReleaseName cfg)
  currentModule <- Module.getMainModule
  Module.getMainModule >>= updateAntecedents packageVersion
  Collect.collectModuleFiles >>= Archive.archive packageVersion
  Module.save currentModule
