module Main.Move.Act.Archive
  ( Handle,
    new,
    archive,
  )
where

import Data.HashMap.Strict qualified as Map
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.External qualified as External
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Archive qualified as Archive
import Main.Move.Scene.Archive.Module.MakeArchiveEns
import Main.Move.Scene.Archive.PackageVersion.ChooseNewVersion qualified as PV
import Main.Move.Scene.Archive.PackageVersion.Reflect qualified as PV
import Main.Move.Scene.Ens.Reflect qualified as EnsReflect
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Save qualified as ModuleSave
import Main.Rule.Config.Archive
import Main.Rule.Module
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    packageVersionHandle :: PV.Handle,
    ensReflectHandle :: EnsReflect.Handle,
    archiveHandle :: Archive.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  let envHandle = Base.envHandle baseHandle
  let packageVersionHandle = PV.new (Base.loggerHandle baseHandle)
  let externalHandle = External.new (Base.loggerHandle baseHandle)
  let moduleSaveHandle = ModuleSave.new (Base.loggerHandle baseHandle)
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
