module Command.Create.Internal
  ( Handle,
    new,
    createNewProject,
    constructDefaultModule,
  )
where

import App.App (App)
import App.Run (raiseError')
import CodeParser.Parser qualified as CP
import Command.Common.SaveModule qualified as SaveModule
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.Allocator (defaultAllocator)
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Const
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Kernel.Common.Platform qualified as P
import Kernel.Common.Target
import Kernel.Common.ZenConfig
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Logger.Handle qualified as Logger
import Logger.Print qualified as Logger
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

createNewProject :: Handle -> T.Text -> Module -> App ()
createNewProject h moduleName newModule = do
  let moduleDir = parent $ moduleLocation newModule
  Path.ensureNotFile "The module destination" moduleDir
  moduleDirExists <- doesDirExist moduleDir
  if moduleDirExists
    then raiseError' $ "The directory `" <> moduleName <> "` already exists"
    else do
      createModuleFile h newModule
      liftIO $ createMainFile newModule
      liftIO $ Logger.printNote' (loggerHandle h) $ "Created a module: " <> moduleName

constructDefaultModule :: T.Text -> Maybe T.Text -> App Module
constructDefaultModule moduleName mTargetName = do
  let targetName = fromMaybe moduleName mTargetName
  ensureValidTargetName targetName
  currentDir <- getCurrentDir
  moduleRelDir <- case parseRelDir (T.unpack moduleName) of
    Just relDir ->
      return relDir
    Nothing ->
      raiseError' $ "Invalid module name: " <> moduleName
  let moduleRootDir = currentDir </> moduleRelDir
  mainFile <- case parseRelFile (T.unpack targetName <> sourceFileExtension) of
    Just relFile ->
      return relFile
    Nothing ->
      raiseError' $ "Invalid target name: " <> targetName
  sourceLocator <- case SL.fromPath mainFile of
    Just locator ->
      return locator
    Nothing ->
      raiseError' $ "The source path `" <> targetName <> "` contains the reserved segment `this`"
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
                  { entryPoint = sourceLocator,
                    clangOption = CL.empty,
                    allocator = defaultAllocator,
                    platform = P.SelectHost,
                    executeCommand = Nothing
                  }
              )
            ],
        moduleZenConfig = ZenConfig {clangOption = CL.empty, allocator = defaultAllocator, platform = P.SelectHost, executeCommand = Nothing},
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleAntecedents = [],
        moduleLocation = moduleRootDir </> moduleFile,
        moduleStaticFiles = Map.empty,
        moduleForeign = Foreign {input = [], output = [], script = []},
        moduleInlineLimit = Nothing,
        moduleUniversal = True,
        modulePresetMap = Map.empty
      }

ensureValidTargetName :: T.Text -> App ()
ensureValidTargetName targetName = do
  when (T.null targetName) $ do
    raiseError' "The target name must not be empty"
  when (T.any (`S.member` CP.nonSymbolCharSet) targetName) $ do
    raiseError' $ "Invalid target name: " <> targetName

createModuleFile :: Handle -> Module -> App ()
createModuleFile h newModule = do
  ensureDir $ parent $ moduleLocation newModule
  SaveModule.save (saveModuleHandle h) (moduleLocation newModule) ([], (toDefaultEns newModule, []))
  buildDir <- Platform.getBaseBuildDir (platformHandle h) newModule
  ensureDir buildDir

createMainFile :: Module -> IO ()
createMainFile newModule = do
  ensureDir $ getSourceDir newModule
  forM_ (getTargetPathList newModule) $ \mainFilePath -> do
    writeText mainFilePath "define main() -> unit {\n  print(\"Hello, world!\\n\");\n}\n"
