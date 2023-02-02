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
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as S
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Entity.OutputKind as OK
import qualified Entity.Source as Source
import qualified Entity.Stmt as Stmt
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
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldCancelAlloc :: Bool
  }

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
    MonadIO m,
    Unravel.Context m
  ) =>
  Context m

build :: Context m => Config -> m ()
build cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Env.setTargetPlatform
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
    Path.ensureNotInLibDir
    Env.setMainModule mainModule
    Env.setShouldCancelAlloc $ shouldCancelAlloc cfg
    case mTarget cfg of
      Just target ->
        build' target mainModule (outputKindList cfg) (shouldSkipLink cfg)
      Nothing -> do
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' target mainModule (outputKindList cfg) (shouldSkipLink cfg)

build' ::
  Context m =>
  Target ->
  Module ->
  [OK.OutputKind] ->
  Bool ->
  m ()
build' target mainModule outputKindList shouldSkipLink = do
  sgl <- resolveTarget mainModule target
  mainFilePath <- Module.getSourcePath sgl
  let mainSource = getMainSource mainModule mainFilePath
  (_, _, isObjectAvailable, dependenceSeq) <- Unravel.unravel mainSource
  Global.initialize
  Parse.parseCachedStmtList Stmt.initialStmtList
  forM_ Stmt.initialStmtList Elaborate.insertStmt
  Clarify.registerFoundationalTypes
  mapM_ (compile outputKindList) dependenceSeq
  isExecutableAvailable <- getExecutableOutputPath target mainModule >>= Path.doesFileExist
  if shouldSkipLink || (isObjectAvailable && isExecutableAvailable)
    then return ()
    else link target mainModule $ toList dependenceSeq

compile ::
  Context m =>
  [OK.OutputKind] ->
  Source.Source ->
  m ()
compile outputKindList source = do
  Env.setCurrentSource source
  Locator.initialize
  b <- isCompilationSkippable outputKindList source
  if b
    then loadTopLevelDefinitions source
    else compile' outputKindList source

isCompilationSkippable :: Context m => [OK.OutputKind] -> Source.Source -> m Bool
isCompilationSkippable outputKindList source =
  case outputKindList of
    [] ->
      return True
    kind : rest -> do
      case kind of
        OK.LLVM -> do
          hasLLVMSet <- Env.getHasLLVMSet
          let b1 = S.member (Source.sourceFilePath source) hasLLVMSet
          b2 <- isCompilationSkippable rest source
          return $ b1 && b2
        OK.Asm ->
          isCompilationSkippable rest source
        OK.Object -> do
          hasObjectSet <- Env.getHasObjectSet
          let b1 = S.member (Source.sourceFilePath source) hasObjectSet
          b2 <- isCompilationSkippable rest source
          return $ b1 && b2

loadTopLevelDefinitions :: Context m => Source.Source -> m ()
loadTopLevelDefinitions source = do
  void $
    Parse.parse source
      >>= Elaborate.elaborate source
      >>= Clarify.clarify source

compile' :: Context m => [OK.OutputKind] -> Source.Source -> m ()
compile' outputKindList source = do
  llvmCode <- compileToLLVM source
  kindPathList <- zipWithM attachOutputPath outputKindList (repeat source)
  forM_ kindPathList $ \(_, outputPath) -> Path.ensureDir $ parent outputPath
  LLVM.emit llvmCode kindPathList

attachOutputPath :: Context m => OK.OutputKind -> Source.Source -> m (OK.OutputKind, Path Abs File)
attachOutputPath outputKind source = do
  outputPath <- Source.sourceToOutputPath outputKind source
  return (outputKind, outputPath)

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
  objectPathList <- mapM (Source.sourceToOutputPath OK.Object) sourceList
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
