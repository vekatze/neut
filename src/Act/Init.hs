module Act.Init
  ( initialize,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Entity.Const
import Entity.Module
import qualified Entity.ModuleID as MID
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Entity.Target
import Path (parent, (</>))

data Config = Config
  { moduleName :: T.Text,
    -- throwCfg :: Throw.Config,
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
  -- throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  -- logCtx <- Mode.logCtx mode $ logCfg cfg
  -- pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
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
        -- moduleCtx <-
        --   Mode.moduleCtx mode $
        --     Module.Config
        --       { Module.mainModule = newModule,
        --         Module.throwCtx = throwCtx,
        --         Module.pathCtx = pathCtx
        --       }
        createMainFile

createModuleFile :: Context m => m ()
createModuleFile = do
  newModule <- Env.getMainModule
  Path.ensureDir $ parent $ moduleLocation newModule
  Path.writeText (moduleLocation newModule) $ ppModule newModule

-- TIO.writeFile (toFilePath $ moduleLocation newModule) $ ppModule newModule

createMainFile :: Context m => m ()
createMainFile = do
  newModule <- Env.getMainModule
  forM_ (Map.elems $ moduleTarget newModule) $ \sgl -> do
    mainFilePath <- Module.getSourcePath sgl
    Path.writeText mainFilePath "define main() : i64 as\n  0\nend\n"

-- TIO.writeFile (toFilePath mainFilePath) "define main() : i64 as\n  0\nend\n"

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
