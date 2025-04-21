module Act.Archive (archive) where

import Context.App
import Context.Env (getMainModule)
import Context.Path qualified as Path
import Rule.Config.Archive
import Scene.Archive qualified as Archive
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize
import Scene.Module.MakeArchiveEns
import Scene.PackageVersion.ChooseNewVersion qualified as PV
import Scene.PackageVersion.Reflect qualified as PV

archive :: Config -> App ()
archive cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInDependencyDir
  packageVersion <- maybe PV.chooseNewVersion PV.reflect (getArchiveName cfg)
  archiveEns <- getMainModule >>= makeArchiveEns packageVersion
  mainModule <- getMainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  Archive.archive packageVersion archiveEns moduleRootDir contents
