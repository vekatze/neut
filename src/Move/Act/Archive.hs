module Move.Act.Archive
  ( Handle,
    new,
    archive,
  )
where

import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Archive qualified as Archive
import Move.Scene.Collect qualified as Collect
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Module.MakeArchiveEns
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.PackageVersion.Reflect qualified as PV
import Rule.Config.Archive

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    envHandle :: Env.Handle,
    packageVersionHandle :: PV.Handle,
    ensReflectHandle :: EnsReflect.Handle,
    archiveHandle :: Archive.Handle
  }

new :: InitCompiler.Handle -> Env.Handle -> PV.Handle -> EnsReflect.Handle -> Archive.Handle -> Handle
new initCompilerHandle envHandle packageVersionHandle ensReflectHandle archiveHandle = do
  Handle {..}

archive :: Handle -> Config -> EIO ()
archive h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  packageVersion <-
    maybe (PV.chooseNewVersion (packageVersionHandle h) mainModule) (PV.reflect mainModule) (getArchiveName cfg)
  archiveEns <- makeArchiveEns (ensReflectHandle h) packageVersion mainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  Archive.archive (archiveHandle h) packageVersion archiveEns moduleRootDir contents
