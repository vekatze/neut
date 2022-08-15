module Act.Build
  ( build,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as S
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.OutputKind
import qualified Entity.Source as Source
import qualified Entity.StrictGlobalLocator as SGL
import Entity.Target
import Path
import qualified Scene.Clarify as Clarify
import qualified Scene.Elaborate as Elaborate
import qualified Scene.Emit as Emit
import qualified Scene.Lower as Lower
import qualified Scene.Parse as Parse
import qualified Scene.Unravel as Unravel
import Prelude hiding (log)

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
    shouldCancelAlloc :: Bool
  }

-- logCfg :: Log.Config,
-- throwCfg :: Throw.Config,

-- data Context = Context
--   { elaborateCtx :: Elaborate.Context,
--     appCtx :: App.Context,
--     llvmCtx :: LLVM.Context,
--     throwCtx :: Throw.Context,
--     logCtx :: Log.Context,
--     setCancellAllocFlag :: Bool -> m (),
--     setCacheSet :: S.Set (Path Abs File) -> m (),
--     setSourceAliasMap :: SourceAliasMap -> m (),
--     setCurrentSource :: Source -> m ()
--   }

class
  ( LLVM.Context m,
    Throw.Context m,
    Log.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    Parse.Context m,
    Elaborate.Context m,
    Clarify.Context m,
    Lower.Context m,
    Emit.Context m,
    Unravel.Context m
  ) =>
  Context m

-- runAndCatch :: m a -> m a

build :: Context m => Config -> m ()
build cfg = do
  -- pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  -- (throwCtx) (Log.printLog (logCtx))
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
    -- moduleCtx <-
    --   Mode.moduleCtx mode $
    --     Module.Config
    --       { Module.mainModule = mainModule,
    --         Module.throwCtx = throwCtx,
    --         Module.pathCtx = pathCtx
    --       }
    Path.ensureNotInLibDir
    Env.setMainModule mainModule
    Env.setShouldCancelAlloc $ shouldCancelAlloc cfg
    case mTarget cfg of
      Just target ->
        build' target mainModule
      Nothing -> do
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' target mainModule

build' ::
  Context m =>
  Target ->
  Module ->
  m ()
build' target mainModule = do
  sgl <- resolveTarget mainModule target
  mainFilePath <- Module.getSourcePath sgl
  let mainSource = getMainSource mainModule mainFilePath
  -- hasCacheSet, hasObjectSet, sourceAliasMap,
  (_, isObjectAvailable, dependenceSeq) <- Unravel.unravel mainSource
  -- setCacheSet hasCacheSet
  -- setSourceAliasMap sourceAliasMap
  Global.initialize
  mapM_ compile dependenceSeq
  -- mapM_ (compile hasObjectSet) dependenceSeq
  unless isObjectAvailable $ link target mainModule $ toList dependenceSeq

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
--           App.gensymCtx = gensymCtx,
--           App.pathCtx = pathCtx,
--           App.implicitCtx = implicitCtx,
--           App.definitionCtx = definitionCtx,
--           App.compDefinitionCtx = compDefinitionCtx,
--           App.typeCtx = typeCtx,
--           App.globalCtx = globalCtx,
--           App.cancelAllocFlagConf = cancelAllocFlag,
--           App.mainModuleConf = mainModule,
--           App.sourceAliasMapConf = sourceAliasMap,
--           App.hasCacheSetConf = hasCacheSet
--         }
-- llvmCtx <- Mode.llvmCtx mode $ LLVM.Config {LLVM.throwCtx = throwCtx, LLVM.clangOptString = ""} -- fixme

compile ::
  Context m =>
  Source.Source ->
  m ()
compile source = do
  Env.setCurrentSource source
  Locator.initialize
  -- <- App.newCfg source
  hasObjectSet <- Env.getHasObjectSet
  if S.member (Source.sourceFilePath source) hasObjectSet
    then loadTopLevelDefinitions source
    else compile' source

loadTopLevelDefinitions :: Context m => Source.Source -> m ()
loadTopLevelDefinitions source = do
  void $
    Parse.parse source
      >>= Elaborate.elaborate source
      >>= Clarify.clarify source

compile' :: Context m => Source.Source -> m ()
compile' source = do
  llvmCode <- compileToLLVM source
  outputPath <- Source.sourceToOutputPath OutputKindObject source
  Path.ensureDir $ parent outputPath
  llvmOutputPath <- Source.sourceToOutputPath OutputKindLLVM source
  Path.writeByteString llvmOutputPath llvmCode
  -- L.writeFile (toFilePath llvmOutputPath) llvmCode
  LLVM.emit OutputKindObject llvmCode outputPath

compileToLLVM :: Context m => Source.Source -> m L.ByteString
compileToLLVM source = do
  Parse.parse source
    >>= Elaborate.elaborate source
    >>= Clarify.clarify source
    >>= Lower.lower
    >>= Emit.emit

link :: Context m => Target -> Module -> [Source.Source] -> m ()
link target mainModule sourceList = do
  outputPath <- getExecutableOutputPath target mainModule
  objectPathList <- mapM (Source.sourceToOutputPath OutputKindObject) sourceList
  LLVM.link objectPathList outputPath

getMainSource :: Module -> Path Abs File -> Source.Source
getMainSource mainModule mainSourceFilePath = do
  Source.Source
    { Source.sourceModule = mainModule,
      Source.sourceFilePath = mainSourceFilePath
    }

resolveTarget :: Throw.Context m => Module -> Target -> m SGL.StrictGlobalLocator
resolveTarget mainModule target = do
  case Map.lookup target (moduleTarget mainModule) of
    Just path ->
      return path
    Nothing ->
      Throw.raiseError' $ "no such target is defined: `" <> extract target <> "`"
