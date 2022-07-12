module Act.Build
  ( build,
    Config (..),
  )
where

import qualified Context.App as App
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
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
import Entity.Target
import Path
import Path.IO
import Scene.Clarify
import Scene.Elaborate
import Scene.Emit
import Scene.Lower
import Scene.Parse
import Scene.Unravel
import System.Exit
import Prelude hiding (log)

type TargetString =
  T.Text

data Config = Config
  { mTarget :: Maybe TargetString,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
    throwCfg :: Throw.Config,
    pathCfg :: Path.Config,
    shouldCancelAlloc :: Bool
  }

build :: Mode.Mode -> Config -> IO ()
build mode cfg = do
  throwCtx <- Mode.throwCtx mode $ throwCfg cfg
  logCtx <- Mode.logCtx mode $ logCfg cfg
  pathCtx <- Mode.pathCtx mode $ pathCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    ensureNotInLibDir throwCtx pathCtx "build"
    case mTarget cfg of
      Just targetString ->
        build' mode throwCtx logCtx moduleCtx (shouldCancelAlloc cfg) (Target targetString) mainModule
      Nothing -> do
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' mode throwCtx logCtx moduleCtx (shouldCancelAlloc cfg) target mainModule

build' ::
  Mode.Mode ->
  Throw.Context ->
  Log.Context ->
  Module.Context ->
  Bool ->
  Target ->
  Module ->
  IO ()
build' mode throwCtx logCtx moduleCtx cancelAllocFlag target mainModule = do
  mainFilePath <- resolveTarget throwCtx mainModule target
  mainSource <- getMainSource mainModule mainFilePath
  (_, isObjectAvailable, hasCacheSet, hasObjectSet, sourceAliasMap, dependenceSeq) <- unravel mode throwCtx moduleCtx mainModule mainSource
  let ctxCfg =
        App.Config
          { App.mode = mode,
            App.throwCtx = throwCtx,
            App.logCtx = logCtx,
            App.cancelAllocFlagConf = cancelAllocFlag,
            App.mainModuleConf = mainModule,
            App.initialSourceConf = mainSource,
            App.sourceAliasMapConf = sourceAliasMap,
            App.hasCacheSetConf = hasCacheSet
          }
  mapM_ (compile ctxCfg hasObjectSet) dependenceSeq
  llvmCtx <- Mode.llvmCtx mode $ LLVM.Config {LLVM.throwCtx = throwCtx, LLVM.clangOptString = ""} -- fixme
  unless isObjectAvailable $ link llvmCtx target mainModule $ toList dependenceSeq

ensureNotInLibDir :: Throw.Context -> Path.Context -> T.Text -> IO ()
ensureNotInLibDir throwCtx pathCtx commandName = do
  currentDir <- getCurrentDir
  libDir <- Path.getLibraryDirPath pathCtx
  when (isProperPrefixOf libDir currentDir) $
    Throw.raiseError' throwCtx $
      "the subcommand `" <> commandName <> "` cannot be run under the library directory"

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
  mMainFunctionName <- Locator.getMainFunctionName (App.locator ctx) source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain ctx mainName source >>= elaborateMain ctx mainName source >>= clarifyMain ctx mainName
    Nothing ->
      void $ parseOther ctx source >>= elaborateOther ctx source >>= clarifyOther ctx

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
  mMainFunctionName <- Locator.getMainFunctionName (App.locator ctx) source
  case mMainFunctionName of
    Just mainName -> do
      parseMain ctx mainName source
        >>= elaborateMain ctx mainName source
        >>= clarifyMain ctx mainName
        >>= lowerMain ctx
        >>= emitMain ctx
    Nothing -> do
      parseOther ctx source
        >>= elaborateOther ctx source
        >>= clarifyOther ctx
        >>= lowerOther ctx
        >> emitOther ctx

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

resolveTarget :: Throw.Context -> Module -> Target -> IO (Path Abs File)
resolveTarget ctx mainModule target = do
  case getTargetFilePath mainModule target of
    Just path ->
      return path
    Nothing -> do
      _ <- Throw.raiseError' ctx $ "no such target is defined: `" <> extract target <> "`"
      exitWith (ExitFailure 1)
