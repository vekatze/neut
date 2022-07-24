module Act.Check
  ( check,
    Config (..),
  )
where

import qualified Context.App as App
import qualified Context.CompDefinition as CompDefinition
import qualified Context.Definition as Definition
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Implicit as Implicit
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.Source
import qualified Entity.StrictGlobalLocator as SGL
import Path
import Scene.Elaborate
import Scene.Parse
import Scene.Unravel

data Config = Config
  { mFilePathString :: Maybe FilePath,
    logCfg :: Log.Config,
    throwCfg :: Throw.Config
  }

check :: Mode.Mode -> Config -> IO ()
check mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  Throw.run throwCtx (Log.printLog logCtx) $ do
    Path.ensureNotInLibDir pathCtx
    mainModule <- Module.fromCurrentPath throwCtx
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    case mFilePathString cfg of
      Just filePathStr -> do
        sgl <- SGL.reflectInMainModule filePathStr
        check' mode throwCtx logCtx pathCtx moduleCtx sgl mainModule
      Nothing -> do
        forM_ (Map.elems $ moduleTarget mainModule) $ \sgl ->
          check' mode throwCtx logCtx pathCtx moduleCtx sgl mainModule

check' ::
  Mode.Mode ->
  Throw.Context ->
  Log.Context ->
  Path.Context ->
  Module.Context ->
  SGL.StrictGlobalLocator ->
  Module ->
  IO ()
check' mode throwCtx logCtx pathCtx moduleCtx sgl mainModule = do
  filePath <- Module.getSourcePath moduleCtx sgl
  ensureFileModuleSanity throwCtx filePath mainModule
  let initialSource = Source {sourceModule = mainModule, sourceFilePath = filePath}
  (_, _, hasCacheSet, _, sourceAliasMap, dependenceSeq) <- unravel mode throwCtx pathCtx moduleCtx mainModule initialSource
  globalCtx <- Mode.globalCtx mode $ Global.Config {Global.throwCtx = throwCtx}
  gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  typeCtx <- Mode.typeCtx mode $ Type.Config {Type.throwCtx = throwCtx}
  implicitCtx <- Mode.implicitCtx mode $ Implicit.Config {}
  definitionCtx <- Mode.definitionCtx mode $ Definition.Config {}
  compDefinitionCtx <- Mode.compDefinitionCtx mode $ CompDefinition.Config {}
  let ctxCfg =
        App.Config
          { App.mode = mode,
            App.throwCtx = throwCtx,
            App.logCtx = logCtx,
            App.globalCtx = globalCtx,
            App.pathCtx = pathCtx,
            App.typeCtx = typeCtx,
            App.implicitCtx = implicitCtx,
            App.definitionCtx = definitionCtx,
            App.compDefinitionCtx = compDefinitionCtx,
            App.gensymCtx = gensymCtx,
            App.cancelAllocFlagConf = False,
            App.mainModuleConf = mainModule,
            App.initialSourceConf = initialSource,
            App.sourceAliasMapConf = sourceAliasMap,
            App.hasCacheSetConf = hasCacheSet
          }
  forM_ dependenceSeq $ \source -> do
    ctx <- App.new ctxCfg source
    void $ parse ctx source >>= elaborate ctx source

ensureFileModuleSanity :: Throw.Context -> Path Abs File -> Module -> IO ()
ensureFileModuleSanity ctx filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' ctx "the specified file is not in the current module"
