module Act.Build
  ( build,
    Config (..),
  )
where

import qualified Context.App as App
import qualified Context.Definition as Definition
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Implicit as Implicit
import qualified Context.LLVM as LLVM
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.OutputKind
import Entity.Source
import qualified Entity.StrictGlobalLocator as SGL
import Entity.Target
import Path
import Path.IO
import Scene.Clarify
import Scene.Elaborate
import Scene.Emit
import Scene.Lower
import Scene.Parse
import Scene.Unravel
import Prelude hiding (log)

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
    throwCfg :: Throw.Config,
    shouldCancelAlloc :: Bool
  }

build :: Mode.Mode -> Config -> IO ()
build mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  pathCtx <- Mode.pathCtx mode $ Path.Config {Path.throwCtx = throwCtx}
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    Path.ensureNotInLibDir pathCtx
    case mTarget cfg of
      Just target ->
        build' mode throwCtx logCtx pathCtx moduleCtx (shouldCancelAlloc cfg) target mainModule
      Nothing -> do
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' mode throwCtx logCtx pathCtx moduleCtx (shouldCancelAlloc cfg) target mainModule

build' ::
  Mode.Mode ->
  Throw.Context ->
  Log.Context ->
  Path.Context ->
  Module.Context ->
  Bool ->
  Target ->
  Module ->
  IO ()
build' mode throwCtx logCtx pathCtx moduleCtx cancelAllocFlag target mainModule = do
  sgl <- resolveTarget throwCtx mainModule target
  mainFilePath <- Module.getSourcePath moduleCtx sgl
  mainSource <- getMainSource mainModule mainFilePath
  (_, isObjectAvailable, hasCacheSet, hasObjectSet, sourceAliasMap, dependenceSeq) <- unravel mode throwCtx pathCtx moduleCtx mainModule mainSource
  globalCtx <- Mode.globalCtx mode $ Global.Config {Global.throwCtx = throwCtx}
  gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  typeCtx <- Mode.typeCtx mode $ Type.Config {Type.throwCtx = throwCtx}
  implicitCtx <- Mode.implicitCtx mode $ Implicit.Config {}
  definitionCtx <- Mode.definitionCtx mode $ Definition.Config {}
  let ctxCfg =
        App.Config
          { App.mode = mode,
            App.throwCtx = throwCtx,
            App.logCtx = logCtx,
            App.gensymCtx = gensymCtx,
            App.pathCtx = pathCtx,
            App.implicitCtx = implicitCtx,
            App.definitionCtx = definitionCtx,
            App.typeCtx = typeCtx,
            App.globalCtx = globalCtx,
            App.cancelAllocFlagConf = cancelAllocFlag,
            App.mainModuleConf = mainModule,
            App.initialSourceConf = mainSource,
            App.sourceAliasMapConf = sourceAliasMap,
            App.hasCacheSetConf = hasCacheSet
          }
  mapM_ (compile ctxCfg hasObjectSet) dependenceSeq
  llvmCtx <- Mode.llvmCtx mode $ LLVM.Config {LLVM.throwCtx = throwCtx, LLVM.clangOptString = ""} -- fixme
  unless isObjectAvailable $ link llvmCtx target mainModule $ toList dependenceSeq

compile ::
  App.Config ->
  S.Set (Path Abs File) ->
  Source ->
  IO ()
compile ctxCfg hasObjectSet source = do
  ctx <- App.new ctxCfg source
  if S.member (sourceFilePath source) hasObjectSet
    then loadTopLevelDefinitions ctx source
    else compile' ctx source

loadTopLevelDefinitions :: App.Context -> Source -> IO ()
loadTopLevelDefinitions ctx source = do
  void $ parse ctx source >>= elaborate ctx source >>= clarify ctx source

compile' :: App.Context -> Source -> IO ()
compile' ctx source = do
  llvmCode <- compileToLLVM ctx source
  outputPath <- sourceToOutputPath OutputKindObject source
  ensureDir $ parent outputPath
  llvmOutputPath <- sourceToOutputPath OutputKindLLVM source
  L.writeFile (toFilePath llvmOutputPath) llvmCode
  LLVM.emit (App.llvm ctx) OutputKindObject llvmCode outputPath

compileToLLVM :: App.Context -> Source -> IO L.ByteString
compileToLLVM ctx source = do
  parse ctx source
    >>= elaborate ctx source
    >>= clarify ctx source
    >>= lower ctx
    >>= emit ctx

link :: LLVM.Context -> Target -> Module -> [Source] -> IO ()
link ctx target mainModule sourceList = do
  outputPath <- getExecutableOutputPath target mainModule
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  LLVM.link ctx objectPathList outputPath

getExecutableOutputPath :: Target -> Module -> IO (Path Abs File)
getExecutableOutputPath target mainModule =
  resolveFile (getExecutableDir mainModule) $ T.unpack $ extract target

getMainSource :: Module -> Path Abs File -> IO Source
getMainSource mainModule mainSourceFilePath = do
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

resolveTarget :: Throw.Context -> Module -> Target -> IO SGL.StrictGlobalLocator
resolveTarget ctx mainModule target = do
  case Map.lookup target (moduleTarget mainModule) of
    Just path ->
      return path
    Nothing ->
      Throw.raiseError' ctx $ "no such target is defined: `" <> extract target <> "`"
