module Act.Init
  ( initialize,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Global
import Entity.Module
import Entity.Target
import Path
import Path.IO

data Config = Config
  { moduleName :: T.Text,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

initialize :: Mode.Mode -> Config -> IO ()
initialize mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    newModule <- constructDefaultModule (moduleName cfg)
    moduleDirExists <- doesDirExist $ parent $ moduleLocation newModule
    if moduleDirExists
      then do
        Throw.raiseError' throwCtx $ "the directory `" <> moduleName cfg <> "` already exists"
      else do
        ensureDir $ parent $ moduleLocation newModule
        ensureDir $ getReleaseDir newModule
        ensureDir $ getSourceDir newModule
        createModuleFile newModule
        createMainFile newModule

createModuleFile :: Module -> IO ()
createModuleFile newModule = do
  ensureDir $ parent $ moduleLocation newModule
  TIO.writeFile (toFilePath $ moduleLocation newModule) $ ppModule newModule

createMainFile :: Module -> IO ()
createMainFile newModule = do
  let sourceDir = getSourceDir newModule
  let target = Map.toList $ moduleTarget newModule
  forM_ target $ \(_, relPath) -> do
    let mainFilePath = sourceDir </> relPath
    TIO.writeFile (toFilePath mainFilePath) "define main() : i64 as\n  0\nend\n"

constructDefaultModule :: T.Text -> IO Module
constructDefaultModule name = do
  currentDir <- getCurrentDir
  moduleRootDir <- resolveDir currentDir $ T.unpack name
  mainFile <- parseRelFile $ T.unpack $ name <> "." <> sourceFileExtension
  return $
    Module
      { moduleTarget = Map.fromList [(Target name, mainFile)],
        moduleDependency = Map.empty,
        moduleExtraContents = [],
        moduleLocation = moduleRootDir </> moduleFile
      }
