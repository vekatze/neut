module Move.Act.Archive (archive) where

import Move.Context.App
import Move.Context.Env (getMainModule)
import Move.Context.Path qualified as Path
import Rule.Config.Archive
import Move.Scene.Archive qualified as Archive
import Move.Scene.Collect qualified as Collect
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.MakeArchiveEns
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.PackageVersion.Reflect qualified as PV

archive :: Config -> App ()
archive cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInDependencyDir
  packageVersion <- maybe PV.chooseNewVersion PV.reflect (getArchiveName cfg)
  archiveEns <- getMainModule >>= makeArchiveEns packageVersion
  mainModule <- getMainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  Archive.archive packageVersion archiveEns moduleRootDir contents
