module Command.Archive.Archive
  ( Handle,
    new,
    archive,
  )
where

import App.App (App)
import Command.Archive.Internal qualified as Archive
import Command.Archive.Module.MakeArchiveEns
import Command.Archive.PackageVersion.ChooseNewVersion qualified as PV
import Command.Archive.PackageVersion.Reflect qualified as PV
import Command.Common.SaveModule qualified as SaveModule
import CommandParser.Config.Archive
import Data.HashMap.Strict qualified as Map
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Module
import Kernel.Common.RunProcess qualified as RunProcess
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
  let runProcessHandle = RunProcess.new (Global.loggerHandle globalHandle)
  let saveModuleHandle = SaveModule.new (Global.loggerHandle globalHandle)
  let archiveHandle = Archive.new runProcessHandle saveModuleHandle envHandle
  Handle {..}

archive :: Handle -> Config -> App ()
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
