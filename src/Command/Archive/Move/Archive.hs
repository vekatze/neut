module Command.Archive.Move.Archive
  ( Handle,
    new,
    archive,
  )
where

import Command.Archive.Move.Internal.Archive qualified as Archive
import Command.Archive.Move.Internal.Module.MakeArchiveEns
import Command.Archive.Move.Internal.PackageVersion.ChooseNewVersion qualified as PV
import Command.Archive.Move.Internal.PackageVersion.Reflect qualified as PV
import Command.Common.Move.SaveModule qualified as SaveModule
import CommandParser.Rule.Config.Archive
import Data.HashMap.Strict qualified as Map
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Module
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.Global.Path qualified as Path
import Kernel.Move.Context.ProcessRunner qualified as ProcessRunner
import Kernel.Move.Scene.Init.Global qualified as Global
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    packageVersionHandle :: PV.Handle,
    archiveHandle :: Archive.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  let envHandle = Global.envHandle globalHandle
  let packageVersionHandle = PV.new (Global.loggerHandle globalHandle)
  let processRunnerHandle = ProcessRunner.new (Global.loggerHandle globalHandle)
  let saveModuleHandle = SaveModule.new (Global.loggerHandle globalHandle)
  let archiveHandle = Archive.new processRunnerHandle saveModuleHandle envHandle
  Handle {..}

archive :: Handle -> Config -> EIO ()
archive h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  packageVersion <-
    maybe (PV.chooseNewVersion (packageVersionHandle h) mainModule) (PV.reflect mainModule) (getArchiveName cfg)
  archiveEns <- makeArchiveEns packageVersion mainModule
  let (moduleRootDir, contents) = collectModuleFiles mainModule
  Archive.archive (archiveHandle h) packageVersion archiveEns moduleRootDir contents

collectModuleFiles :: MainModule -> (Path Abs Dir, [SomePath Rel])
collectModuleFiles (MainModule baseModule) = do
  let moduleRootDir = parent $ moduleLocation baseModule
  let relModuleSourceDir = Left $ moduleSourceDir baseModule
  let foreignContents = input $ moduleForeign baseModule
  let extraContents = moduleExtraContents baseModule
  let staticContents = map (\(_, path) -> Right path) $ Map.toList $ moduleStaticFiles baseModule
  (moduleRootDir, relModuleSourceDir : foreignContents ++ staticContents ++ extraContents)
