module Act.Archive (archive) where

import Context.App
import Context.Module (getMainModule)
import Context.Module qualified as Module
import Context.Path qualified as Path
import Entity.Config.Archive
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize
import Scene.Module.MakeArchiveEns
import Scene.PackageVersion.Reflect qualified as PV

archive :: Config -> App ()
archive cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Path.ensureNotInLibDir
  packageVersion <- PV.reflect (getArchiveName cfg)
  archiveEns <- Module.getMainModule >>= makeArchiveEns packageVersion
  mainModule <- getMainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  Archive.archive packageVersion archiveEns moduleRootDir contents
