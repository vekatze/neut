module Scene.New
  ( createNewProject,
    constructDefaultModule,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Const
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.SourceLocator qualified as SL
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
      Remark.printNote' $ "created a module: " <> moduleName

constructDefaultModule :: T.Text -> App Module
constructDefaultModule name = do
  currentDir <- Path.getCurrentDir
  moduleRootDir <- Path.resolveDir currentDir $ T.unpack name
  mainFile <- Path.parseRelFile $ T.unpack name <> sourceFileExtension
  return $
    Module
      { moduleID = MID.Main,
        moduleArchiveDir = archiveRelDir,
        moduleBuildDir = buildRelDir,
        moduleSourceDir = sourceRelDir,
        moduleTarget =
          Map.fromList
            [ ( name,
                SL.SourceLocator mainFile
              )
            ],
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleAntecedents = [],
        moduleLocation = moduleRootDir </> moduleFile,
        moduleForeign = Foreign {input = [], output = [], script = []},
        modulePrefixMap = Map.empty,
        moduleInlineLimit = Nothing,
        modulePresetMap = Map.empty
      }

createModuleFile :: App ()
createModuleFile = do
  newModule <- Module.getMainModule
  Path.ensureDir $ parent $ moduleLocation newModule
  Module.saveEns (moduleLocation newModule) ([], (toDefaultEns newModule, []))
  buildDir <- Path.getBaseBuildDir newModule
  Path.ensureDir buildDir

createMainFile :: App ()
createMainFile = do
  newModule <- Module.getMainModule
  Path.ensureDir $ getSourceDir newModule
  forM_ (getTargetPathList newModule) $ \mainFilePath -> do
    Path.writeText mainFilePath "define main(): unit {\n  print(\"Hello, world!\\n\")\n}\n"
