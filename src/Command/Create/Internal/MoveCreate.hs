module Command.Create.Internal.MoveCreate
  ( Handle,
    new,
    createNewProject,
    constructDefaultModule,
  )
where

import Command.Common.SaveModule qualified as SaveModule
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError')
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Const
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Kernel.Common.Target
import Kernel.Common.ZenConfig
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Logger.Handle qualified as Logger
import Logger.MoveLog qualified as Logger
import Path
import Path.IO
import Path.Write (writeText)

data Handle = Handle
  { saveModuleHandle :: SaveModule.Handle,
    platformHandle :: Platform.Handle,
    loggerHandle :: Logger.Handle
  }

new :: SaveModule.Handle -> Logger.Handle -> Platform.Handle -> IO Handle
new saveModuleHandle loggerHandle platformHandle = do
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
        moduleInlineLimit = Nothing,
        modulePresetMap = Map.empty
      }

createModuleFile :: Handle -> Module -> EIO ()
createModuleFile h newModule = do
  ensureDir $ parent $ moduleLocation newModule
  SaveModule.save (saveModuleHandle h) (moduleLocation newModule) ([], (toDefaultEns newModule, []))
  buildDir <- Platform.getBaseBuildDir (platformHandle h) newModule
  ensureDir buildDir

createMainFile :: Module -> IO ()
createMainFile newModule = do
  ensureDir $ getSourceDir newModule
  forM_ (getTargetPathList newModule) $ \mainFilePath -> do
    writeText mainFilePath "define main(): unit {\n  print(\"Hello, world!\\n\");\n}\n"
