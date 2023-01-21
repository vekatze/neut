module Act.Build
  ( build,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.LLVM qualified as LLVM
import Context.Locator qualified as Locator
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.ByteString.Lazy qualified as L
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Entity.Module
import Entity.Module.Reflect qualified as Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Stmt qualified as Stmt
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Scene.Clarify qualified as Clarify
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import Prelude hiding (log)

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
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
    Unravel.Context m
  ) =>
  Context m

build :: Context m => Config -> m ()
build cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    mainModule <- Module.fromCurrentPath
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
  (_, isObjectAvailable, dependenceSeq) <- Unravel.unravel mainSource
  Global.initialize
  Parse.parseCachedStmtList Stmt.initialStmtList
  forM_ Stmt.initialStmtList Elaborate.insertStmt
  Clarify.registerFoundationalTypes
  mapM_ compile dependenceSeq
  unless isObjectAvailable $ link target mainModule $ toList dependenceSeq

compile ::
  Context m =>
  Source.Source ->
  m ()
compile source = do
  Env.setCurrentSource source
  Locator.initialize
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
  outputPath <- Source.sourceToOutputPath OK.Object source
  Path.ensureDir $ parent outputPath
  llvmOutputPath <- Source.sourceToOutputPath OK.LLVM source
  Path.writeByteString llvmOutputPath llvmCode
  LLVM.emit OK.Object llvmCode outputPath

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
