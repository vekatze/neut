module Main.Move.Scene.Create
  ( Handle,
    new,
    createNewProject,
    constructDefaultModule,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError')
import Language.Common.Rule.ModuleID qualified as MID
import Language.Common.Rule.SourceLocator qualified as SL
import Logger.Move.Log qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Scene.Module.Save qualified as ModuleSave
import Main.Rule.ClangOption qualified as CL
import Main.Rule.Const
import Main.Rule.Module
import Main.Rule.Target
import Main.Rule.ZenConfig
import Path
import Path.IO

data Handle = Handle
  { moduleSaveHandle :: ModuleSave.Handle,
    platformHandle :: Platform.Handle,
    loggerHandle :: Logger.Handle
  }

new :: ModuleSave.Handle -> Logger.Handle -> Platform.Handle -> IO Handle
new moduleSaveHandle loggerHandle platformHandle = do
  return $ Handle {..}

createNewProject :: Handle -> T.Text -> Module -> EIO ()
createNewProject h moduleName newModule = do
  let moduleDir = parent $ moduleLocation newModule
  moduleDirExists <- doesDirExist moduleDir
  if moduleDirExists
    then raiseError' $ "The directory `" <> moduleName <> "` already exists"
    else do
      createModuleFile h newModule
      liftIO $ createMainFile newModule
      liftIO $ Logger.printNote' (loggerHandle h) $ "Created a module: " <> moduleName

constructDefaultModule :: T.Text -> Maybe T.Text -> EIO Module
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

createModuleFile :: Handle -> Module -> EIO ()
createModuleFile h newModule = do
  ensureDir $ parent $ moduleLocation newModule
  ModuleSave.save (moduleSaveHandle h) (moduleLocation newModule) ([], (toDefaultEns newModule, []))
  buildDir <- Platform.getBaseBuildDir (platformHandle h) newModule
  ensureDir buildDir

createMainFile :: Module -> IO ()
createMainFile newModule = do
  ensureDir $ getSourceDir newModule
  forM_ (getTargetPathList newModule) $ \mainFilePath -> do
    Path.writeText mainFilePath "define main(): unit {\n  print(\"Hello, world!\\n\")\n}\n"
