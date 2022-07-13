module Act.Init
  ( initialize,
    Config (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Const
import Entity.Module
import qualified Entity.ModuleID as MID
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Entity.Target
import Path
import Path.IO

data Config = Config
  { moduleName :: T.Text,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config,
    pathCfg :: Path.Config
  }

initialize :: Mode.Mode -> Config -> IO ()
initialize mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  pathCtx <- Mode.pathCtx mode $ pathCfg cfg
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
        moduleCtx <-
          Mode.moduleCtx mode $
            Module.Config
              { Module.mainModule = newModule,
                Module.throwCtx = throwCtx,
                Module.pathCtx = pathCtx
              }
        createMainFile moduleCtx newModule

createModuleFile :: Module -> IO ()
createModuleFile newModule = do
  ensureDir $ parent $ moduleLocation newModule
  TIO.writeFile (toFilePath $ moduleLocation newModule) $ ppModule newModule

createMainFile :: Module.Context -> Module -> IO ()
createMainFile moduleCtx newModule = do
  -- let sourceDir = getSourceDir newModule
  -- let target = Map.toList $ moduleTarget newModule
  forM_ (Map.elems $ moduleTarget newModule) $ \sgl -> do
    mainFilePath <- Module.getSourcePath moduleCtx sgl
    -- let mainFilePath = sourceDir </> relPath
    TIO.writeFile (toFilePath mainFilePath) "define main() : i64 as\n  0\nend\n"

constructDefaultModule :: T.Text -> IO Module
constructDefaultModule name = do
  currentDir <- getCurrentDir
  moduleRootDir <- resolveDir currentDir $ T.unpack name
  mainFile <- parseRelFile $ T.unpack name <> sourceFileExtension
  return $
    Module
      { moduleTarget =
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
        moduleLocation = moduleRootDir </> moduleFile
      }
