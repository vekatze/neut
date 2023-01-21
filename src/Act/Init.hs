module Act.Init
  ( initialize,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Log qualified as Log
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

data Config = Config
  { moduleName :: T.Text,
    logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    Module.Context m
  ) =>
  Context m

initialize :: Context m => Config -> m ()
initialize cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    newModule <- constructDefaultModule (moduleName cfg)
    Env.setMainModule newModule
    moduleDirExists <- Path.doesDirExist $ parent $ moduleLocation newModule
    if moduleDirExists
      then do
        Throw.raiseError' $ "the directory `" <> moduleName cfg <> "` already exists"
      else do
        Path.ensureDir $ parent $ moduleLocation newModule
        Path.ensureDir $ getSourceDir newModule
        createModuleFile
        createMainFile

createModuleFile :: Context m => m ()
createModuleFile = do
  newModule <- Env.getMainModule
  Path.ensureDir $ parent $ moduleLocation newModule
  Path.writeText (moduleLocation newModule) $ ppModule newModule

createMainFile :: Context m => m ()
createMainFile = do
  newModule <- Env.getMainModule
  forM_ (Map.elems $ moduleTarget newModule) $ \sgl -> do
    mainFilePath <- Module.getSourcePath sgl
    Path.writeText mainFilePath "define main(): i64 =\n  0\nend\n"

constructDefaultModule :: Context m => T.Text -> m Module
constructDefaultModule name = do
  currentDir <- Path.getCurrentDir
  moduleRootDir <- Path.resolveDir currentDir $ T.unpack name
  mainFile <- Path.parseRelFile $ T.unpack name <> sourceFileExtension
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
