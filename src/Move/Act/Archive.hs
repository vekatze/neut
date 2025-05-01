module Move.Act.Archive
  ( Handle,
    new,
    archive,
  )
where

import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Move.Scene.Archive qualified as Archive
import Move.Scene.Collect qualified as Collect
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Module.MakeArchiveEns
import Move.Scene.Module.Save qualified as ModuleSave
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.PackageVersion.Reflect qualified as PV
import Rule.Config.Archive

data Handle
  = Handle
  { envHandle :: Env.Handle,
    packageVersionHandle :: PV.Handle,
    ensReflectHandle :: EnsReflect.Handle,
    archiveHandle :: Archive.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  let envHandle = Base.envHandle baseHandle
  let packageVersionHandle = PV.new (Base.reportHandle baseHandle)
  let externalHandle = External.new (Base.debugHandle baseHandle)
  let moduleSaveHandle = ModuleSave.new (Base.debugHandle baseHandle)
  let archiveHandle = Archive.new externalHandle moduleSaveHandle envHandle
  let ensReflectHandle = EnsReflect.new (Base.gensymHandle baseHandle)
  Handle {..}

archive :: Handle -> Config -> EIO ()
archive h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  packageVersion <-
    maybe (PV.chooseNewVersion (packageVersionHandle h) mainModule) (PV.reflect mainModule) (getArchiveName cfg)
  archiveEns <- makeArchiveEns (ensReflectHandle h) packageVersion mainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  Archive.archive (archiveHandle h) packageVersion archiveEns moduleRootDir contents
