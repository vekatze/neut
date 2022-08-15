module Act.Check
  ( check,
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
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Entity.Source as Source
import qualified Entity.StrictGlobalLocator as SGL
import Path
import qualified Scene.Elaborate as Elaborate
import qualified Scene.Parse as Parse
import qualified Scene.Unravel as Unravel

data Config = Config
  { mFilePathString :: Maybe FilePath,
    logCfg :: Log.Config
    -- throwCfg :: Throw.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    Unravel.Context m,
    Parse.Context m,
    Elaborate.Context m
  ) =>
  Context m

check :: Context m => Config -> m ()
check cfg = do
  -- throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  -- logCtx <- Mode.logCtx mode $ logCfg cfg
  -- pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    Path.ensureNotInLibDir
    mainModule <- Module.fromCurrentPath
    Env.setMainModule mainModule
    -- moduleCtx <-
    --   Mode.moduleCtx mode $
    --     Module.Config
    --       { Module.mainModule = mainModule,
    --         Module.throwCtx = throwCtx,
    --         Module.pathCtx = pathCtx
    --       }
    case mFilePathString cfg of
      Just filePathStr -> do
        sgl <- SGL.reflectInMainModule filePathStr
        check' sgl mainModule
      Nothing -> do
        forM_ (Map.elems $ moduleTarget mainModule) $ \sgl ->
          check' sgl mainModule

check' ::
  Context m =>
  SGL.StrictGlobalLocator ->
  Module ->
  m ()
check' sgl mainModule = do
  filePath <- Module.getSourcePath sgl
  ensureFileModuleSanity filePath mainModule
  let initialSource = Source.Source {Source.sourceModule = mainModule, Source.sourceFilePath = filePath}
  (_, _, dependenceSeq) <- Unravel.unravel initialSource
  -- globalCtx <- Mode.globalCtx mode $ Global.Config {Global.throwCtx = throwCtx}
  -- gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  -- typeCtx <- Mode.typeCtx mode $ Type.Config {Type.throwCtx = throwCtx}
  -- implicitCtx <- Mode.implicitCtx mode $ Implicit.Config {}
  -- definitionCtx <- Mode.definitionCtx mode $ Definition.Config {}
  -- compDefinitionCtx <- Mode.compDefinitionCtx mode $ CompDefinition.Config {}
  -- letCfg =
  --       App.Config
  --         { App.mode = mode,
  --           App.throwCtx = throwCtx,
  --           App.logCtx = logCtx,
  --           App.globalCtx = globalCtx,
  --           App.pathCtx = pathCtx,
  --           App.typeCtx = typeCtx,
  --           App.implicitCtx = implicitCtx,
  --           App.definitionCtx = definitionCtx,
  --           App.compDefinitionCtx = compDefinitionCtx,
  --           App.gensymCtx = gensymCtx,
  --           App.cancelAllocFlagConf = False,
  --           App.mainModuleConf = mainModule,
  --           App.sourceAliasMapConf = sourceAliasMap,
  --           App.hasCacheSetConf = hasCacheSet
  --         }
  forM_ dependenceSeq $ \source -> do
    -- <- App.newCfg source
    void $ Parse.parse source >>= Elaborate.elaborate source

ensureFileModuleSanity :: Throw.Context m => Path Abs File -> Module -> m ()
ensureFileModuleSanity filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' "the specified file is not in the current module"
