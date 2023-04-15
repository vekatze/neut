module Scene.New
  ( createNewProject,
    constructDefaultModule,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Const
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path (parent, (</>))

createNewProject :: T.Text -> Module -> App ()
createNewProject moduleName newModule = do
  let moduleDir = parent $ moduleLocation newModule
  moduleDirExists <- Path.doesDirExist moduleDir
  if moduleDirExists
    then Throw.raiseError' $ "the directory `" <> moduleName <> "` already exists"
    else do
      createModuleFile
      createMainFile

constructDefaultModule :: T.Text -> App Module
constructDefaultModule name = do
  currentDir <- Path.getCurrentDir
  moduleRootDir <- Path.resolveDir currentDir $ T.unpack name
  mainFile <- Path.parseRelFile $ T.unpack name <> sourceFileExtension
  return $
    Module
      { moduleID = MID.Main,
        moduleTarget =
          Map.fromList
            [ ( Target name,
                SGL.StrictGlobalLocator
                  { SGL.moduleID = MID.Main,
                    SGL.sourceLocator = SL.SourceLocator mainFile
                  }
              )
            ],
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleAntecedents = [],
        moduleLocation = moduleRootDir </> moduleFile
      }

createModuleFile :: App ()
createModuleFile = do
  newModule <- Module.getMainModule
  Path.ensureDir $ parent $ moduleLocation newModule
  Module.save newModule
  targetDir <- Path.getTargetDir newModule
  Path.ensureDir targetDir

createMainFile :: App ()
createMainFile = do
  newModule <- Module.getMainModule
  Path.ensureDir $ getSourceDir newModule
  forM_ (Map.elems $ moduleTarget newModule) $ \sgl -> do
    mainFilePath <- Module.getSourcePath sgl
    Path.writeText mainFilePath "define main(): i64 {\n  0\n}\n"
