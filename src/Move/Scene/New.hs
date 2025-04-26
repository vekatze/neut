module Move.Scene.New
  ( createNewProject,
    constructDefaultModule,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Path qualified as Path
import Move.Context.Remark qualified as Remark
import Move.Context.Throw qualified as Throw
import Move.Scene.Module.Save qualified as ModuleSave
import Path
import Path.IO
import Rule.ClangOption qualified as CL
import Rule.Const
import Rule.Module
import Rule.ModuleID qualified as MID
import Rule.SourceLocator qualified as SL
import Rule.Target
import Rule.ZenConfig

createNewProject :: T.Text -> Module -> App ()
createNewProject moduleName newModule = do
  let moduleDir = parent $ moduleLocation newModule
  moduleDirExists <- doesDirExist moduleDir
  if moduleDirExists
    then Throw.raiseError' $ "The directory `" <> moduleName <> "` already exists"
    else do
      createModuleFile newModule
      liftIO $ createMainFile newModule
      Remark.printNote' $ "Created a module: " <> moduleName

constructDefaultModule :: T.Text -> Maybe T.Text -> App Module
constructDefaultModule moduleName mTargetName = do
  let targetName = fromMaybe moduleName mTargetName
  currentDir <- getCurrentDir
  moduleRootDir <- resolveDir currentDir $ T.unpack moduleName
  mainFile <- parseRelFile $ T.unpack targetName <> sourceFileExtension
  return $
    Module
      { moduleID = MID.Main,
        moduleArchiveDir = archiveRelDir,
        moduleCacheDir = cacheRelDir,
        moduleSourceDir = sourceRelDir,
        moduleTarget =
          Map.fromList
            [ ( targetName,
                TargetSummary
                  { entryPoint = SL.SourceLocator mainFile,
                    clangOption = CL.empty
                  }
              )
            ],
        moduleZenConfig = ZenConfig {clangOption = CL.empty},
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleAntecedents = [],
        moduleLocation = moduleRootDir </> moduleFile,
        moduleStaticFiles = Map.empty,
        moduleForeign = Foreign {input = [], output = [], script = []},
        modulePrefixMap = Map.empty,
        moduleInlineLimit = Nothing,
        modulePresetMap = Map.empty
      }

createModuleFile :: Module -> App ()
createModuleFile newModule = do
  ensureDir $ parent $ moduleLocation newModule
  h <- ModuleSave.new
  toApp $ ModuleSave.save h (moduleLocation newModule) ([], (toDefaultEns newModule, []))
  buildDir <- toApp $ Path.getBaseBuildDir newModule
  ensureDir buildDir

createMainFile :: Module -> IO ()
createMainFile newModule = do
  ensureDir $ getSourceDir newModule
  forM_ (getTargetPathList newModule) $ \mainFilePath -> do
    Path.writeText mainFilePath "define main(): unit {\n  print(\"Hello, world!\\n\")\n}\n"
