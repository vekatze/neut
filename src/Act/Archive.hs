module Act.Archive (archive) where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Entity.Config.Archive
import Entity.Module
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Ens.Reflect qualified as Ens
import Scene.Initialize qualified as Initialize
import Scene.Module.UpdateAntecedents
import Scene.PackageVersion.Reflect qualified as PV

archive :: Config -> App ()
archive cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Path.ensureNotInLibDir
  packageVersion <- PV.reflect (getArchiveName cfg)
  currentModule <- Module.getMainModule
  baseEns <- Ens.fromFilePath (moduleLocation currentModule)
  Module.getMainModule >>= updateAntecedents packageVersion
  Collect.collectModuleFiles >>= Archive.archive packageVersion
  Module.saveEns (moduleLocation currentModule) baseEns
