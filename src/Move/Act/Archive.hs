module Move.Act.Archive (archive) where

import Move.Console.Report (getColorSpecStdOut)
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.Path qualified as Path
import Move.Scene.Archive qualified as Archive
import Move.Scene.Collect qualified as Collect
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.MakeArchiveEns
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.PackageVersion.Reflect qualified as PV
import Rule.Config.Archive

archive :: Config -> App ()
archive cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  mainModule <- getMainModule
  toApp $ Path.ensureNotInDependencyDir mainModule
  c <- getColorSpecStdOut
  packageVersion <- toApp $ maybe (PV.chooseNewVersion c mainModule) (PV.reflect mainModule) (getArchiveName cfg)
  h <- EnsReflect.new
  archiveEns <- getMainModule >>= toApp . makeArchiveEns h packageVersion
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  h' <- Archive.new
  toApp $ Archive.archive h' packageVersion archiveEns moduleRootDir contents
