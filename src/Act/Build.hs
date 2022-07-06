module Act.Build
  ( build,
    clean,
    check,
    BuildConfig (..),
    CheckConfig (..),
    CleanConfig (..),
  )
where

import qualified Context.Alias as Alias
import qualified Context.App as App
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.LLVM as LLVM
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Global
import Entity.Hint
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.OutputKind
import Entity.Source
import qualified Entity.Target as Target
import Path
import Path.IO
import Scene.Clarify
import Scene.Elaborate
import Scene.Emit
import Scene.Lower
import Scene.Parse
import Scene.Parse.Core
import Scene.Parse.Import
import System.Exit
import System.IO.Unsafe
import qualified System.Info as System
import Prelude hiding (log)

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

data BuildConfig = BuildConfig
  { mTarget :: Maybe TargetString,
    mClangOptString :: Maybe String,
    buildLogCfg :: Log.Config,
    buildThrowCfg :: Throw.Config,
    shouldCancelAlloc :: Bool
  }

build :: Mode.Mode -> BuildConfig -> IO ()
build mode cfg = do
  throwCtx <- Mode.throwCtx mode $ buildThrowCfg cfg
  logCtx <- Mode.logCtx mode $ buildLogCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    ensureNotInLibDir throwCtx "build"
    mainModule <- Module.fromCurrentPath throwCtx
    case mTarget cfg of
      Just target ->
        build' mode throwCtx logCtx (shouldCancelAlloc cfg) target mainModule
      Nothing -> do
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' mode throwCtx logCtx (shouldCancelAlloc cfg) (T.unpack target) mainModule

build' ::
  Mode.Mode ->
  Throw.Context ->
  Log.Context ->
  Bool ->
  TargetString ->
  Module ->
  IO ()
build' mode throwCtx logCtx cancelAllocFlag target mainModule = do
  mainFilePath <- resolveTarget throwCtx mainModule target
  mainSource <- getMainSource mainModule mainFilePath
  (_, isObjectAvailable, dependenceSeq) <- computeDependence throwCtx mainSource
  hasObjectSet <- readIORef hasObjectSetRef
  gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  globalCtx <-
    Mode.globalCtx mode $
      Global.Config
        { Global.throwCtx = throwCtx
        }
  let ctxCfg =
        CC
          { ccMode = mode,
            ccThrowCtx = throwCtx,
            ccLogCtx = logCtx,
            ccGlobalCtx = globalCtx,
            ccGensymCtx = gensymCtx,
            ccCancelAllocFlag = cancelAllocFlag,
            ccMainModule = mainModule,
            ccInitialSource = mainSource
          }
  mapM_ (compile ctxCfg hasObjectSet) dependenceSeq
  llvmCtx <- Mode.llvmCtx mode $ LLVM.Config {LLVM.throwCtx = throwCtx, LLVM.clangOptString = ""} -- fixme
  unless isObjectAvailable $ link llvmCtx target mainModule $ toList dependenceSeq

data ContextConfig = CC
  { ccMode :: Mode.Mode,
    ccThrowCtx :: Throw.Context,
    ccLogCtx :: Log.Context,
    ccGlobalCtx :: Global.Context,
    ccGensymCtx :: Gensym.Context,
    ccCancelAllocFlag :: Bool,
    ccMainModule :: Module,
    ccInitialSource :: Source
  }

newCtx :: ContextConfig -> Source -> IO App.Context
newCtx cfg source = do
  llvmCtx <-
    Mode.llvmCtx (ccMode cfg) $
      LLVM.Config
        { LLVM.throwCtx = ccThrowCtx cfg,
          LLVM.clangOptString = "" -- fixme
        }
  locatorCtx <-
    Mode.locatorCtx (ccMode cfg) $
      Locator.Config
        { Locator.currentSource = source,
          Locator.mainModule = ccMainModule cfg,
          Locator.throwCtx = ccThrowCtx cfg
        }
  aliasCtx <-
    Mode.aliasCtx (ccMode cfg) $
      Alias.Config
        { Alias.currentModule = sourceModule source,
          Alias.mainModule = ccMainModule cfg,
          Alias.throwCtx = ccThrowCtx cfg,
          Alias.locatorCtx = locatorCtx
        }
  return $
    App.Context
      { App.log = ccLogCtx cfg,
        App.throw = ccThrowCtx cfg,
        App.gensym = ccGensymCtx cfg,
        App.llvm = llvmCtx,
        App.global = ccGlobalCtx cfg,
        App.locator = locatorCtx,
        App.alias = aliasCtx,
        App.shouldCancelAlloc = ccCancelAllocFlag cfg,
        App.initialSource = ccInitialSource cfg,
        App.target =
          Target.Target
            { Target.os = System.os,
              Target.arch = System.arch
            }
      }

data CheckConfig = CheckConfig
  { mFilePathString :: Maybe FilePath,
    checkLogCfg :: Log.Config,
    checkThrowCfg :: Throw.Config
  }

check :: Mode.Mode -> CheckConfig -> IO ()
check mode cfg = do
  throwCtx <- Mode.throwCtx mode $ checkThrowCfg cfg
  logCtx <- Mode.logCtx mode $ checkLogCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    ensureNotInLibDir throwCtx "check"
    mainModule <- Module.fromCurrentPath throwCtx
    case mFilePathString cfg of
      Just filePathStr -> do
        filePath <- resolveFile' filePathStr
        check' mode throwCtx logCtx filePath mainModule
      Nothing -> do
        forM_ (Map.elems $ moduleTarget mainModule) $ \relPath ->
          check' mode throwCtx logCtx (getSourceDir mainModule </> relPath) mainModule

check' :: Mode.Mode -> Throw.Context -> Log.Context -> Path Abs File -> Module -> IO ()
check' mode throwCtx logCtx filePath mainModule = do
  ensureFileModuleSanity throwCtx filePath mainModule
  let source = Source {sourceModule = mainModule, sourceFilePath = filePath}
  (_, _, dependenceSeq) <- computeDependence throwCtx source
  globalCtx <- Mode.globalCtx mode $ Global.Config {Global.throwCtx = throwCtx}
  gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  let ctxCfg =
        CC
          { ccMode = mode,
            ccThrowCtx = throwCtx,
            ccLogCtx = logCtx,
            ccGlobalCtx = globalCtx,
            ccGensymCtx = gensymCtx,
            ccCancelAllocFlag = False,
            ccMainModule = mainModule,
            ccInitialSource = source
          }
  mapM_ (check'' ctxCfg) dependenceSeq

ensureFileModuleSanity :: Throw.Context -> Path Abs File -> Module -> IO ()
ensureFileModuleSanity ctx filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' ctx "the specified file is not in the current module"

ensureNotInLibDir :: Throw.Context -> T.Text -> IO ()
ensureNotInLibDir ctx commandName = do
  currentDir <- getCurrentDir
  libDir <- getLibraryDirPath
  when (isProperPrefixOf libDir currentDir) $
    Throw.raiseError' ctx $
      "the subcommand `" <> commandName <> "` cannot be run under the library directory"

check'' :: ContextConfig -> Source -> IO ()
check'' ctxCfg source = do
  ctx <- newCtx ctxCfg source
  mMainFunctionName <- getMainFunctionName ctx source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain ctx mainName source >>= elaborateMain ctx mainName source
    Nothing ->
      void $ parseOther ctx source >>= elaborateOther ctx source

compile ::
  ContextConfig ->
  S.Set (Path Abs File) ->
  Source ->
  IO ()
compile ctxCfg hasObjectSet source = do
  ctx <- newCtx ctxCfg source
  if S.member (sourceFilePath source) hasObjectSet
    then loadTopLevelDefinitions ctx source
    else compile' ctx source

loadTopLevelDefinitions :: App.Context -> Source -> IO ()
loadTopLevelDefinitions ctx source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint ctx source
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
  mMainFunctionName <- getMainFunctionNameIfEntryPoint ctx source
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

link :: LLVM.Context -> TargetString -> Module -> [Source] -> IO ()
link ctx target mainModule sourceList = do
  outputPath <- getExecutableOutputPath target mainModule
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  LLVM.link ctx objectPathList outputPath

data CleanConfig = CleanConfig
  { cleanLogCfg :: Log.Config,
    cleanThrowCfg :: Throw.Config
  }

clean :: Mode.Mode -> CleanConfig -> IO ()
clean mode cfg = do
  throwCtx <- Mode.throwCtx mode $ cleanThrowCfg cfg
  logCtx <- Mode.logCtx mode $ cleanLogCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let targetDir = getTargetDir mainModule
    b <- doesDirExist targetDir
    when b $ removeDirRecur $ getTargetDir mainModule

getExecutableOutputPath :: TargetString -> Module -> IO (Path Abs File)
getExecutableOutputPath target mainModule =
  resolveFile (getExecutableDir mainModule) target

sourceToOutputPath :: OutputKind -> Source -> IO (Path Abs File)
sourceToOutputPath kind source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtensionAlongKind (artifactDir </> relPathWithoutExtension) kind

getMainSource :: Module -> Path Abs File -> IO Source
getMainSource mainModule mainSourceFilePath = do
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

getMainFunctionName :: App.Context -> Source -> IO (Maybe T.Text)
getMainFunctionName ctx source = do
  b <- isMainFile source
  if b
    then return <$> getMainFunctionName' ctx
    else return Nothing

getMainFunctionNameIfEntryPoint :: App.Context -> Source -> IO (Maybe T.Text)
getMainFunctionNameIfEntryPoint ctx source = do
  if sourceFilePath source == sourceFilePath (App.initialSource ctx)
    then return <$> getMainFunctionName' ctx
    else return Nothing

getMainFunctionName' :: App.Context -> IO T.Text
getMainFunctionName' ctx = do
  Locator.attachCurrentLocator (App.locator ctx) "main"

{-# NOINLINE traceSourceListRef #-}
traceSourceListRef :: IORef [Source]
traceSourceListRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE visitEnvRef #-}
visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo)
visitEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE sourceChildrenMapRef #-}
sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source])
sourceChildrenMapRef =
  unsafePerformIO (newIORef Map.empty)

computeDependence :: Throw.Context -> Source -> IO (IsCacheAvailable, IsObjectAvailable, Seq Source)
computeDependence ctx source = do
  visitEnv <- readIORef visitEnvRef
  let path = sourceFilePath source
  case Map.lookup path visitEnv of
    Just VisitInfoActive ->
      raiseCyclicPath ctx source
    Just VisitInfoFinish -> do
      hasCacheSet <- readIORef hasCacheSetRef
      hasObjectSet <- readIORef hasObjectSetRef
      return (path `S.member` hasCacheSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoActive
      modifyIORef' traceSourceListRef $ \sourceList -> source : sourceList
      children <- getChildren ctx source
      (isCacheAvailableList, isObjectAvailableList, seqList) <- unzip3 <$> mapM (computeDependence ctx) children
      modifyIORef' traceSourceListRef tail
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoFinish
      isCacheAvailable <- checkIfCacheIsAvailable isCacheAvailableList source
      isObjectAvailable <- checkIfObjectIsAvailable isObjectAvailableList source
      return (isCacheAvailable, isObjectAvailable, foldl' (><) Seq.empty seqList |> source)

checkIfCacheIsAvailable :: [IsCacheAvailable] -> Source -> IO IsCacheAvailable
checkIfCacheIsAvailable isCacheAvailableList source = do
  b <- isFreshCacheAvailable source
  let isCacheAvailable = and $ b : isCacheAvailableList
  when isCacheAvailable $
    modifyIORef' hasCacheSetRef $ S.insert $ sourceFilePath source
  return isCacheAvailable

checkIfObjectIsAvailable :: [IsObjectAvailable] -> Source -> IO IsObjectAvailable
checkIfObjectIsAvailable isObjectAvailableList source = do
  b <- isFreshObjectAvailable source
  let isObjectAvailable = and $ b : isObjectAvailableList
  when isObjectAvailable $
    modifyIORef' hasObjectSetRef $ S.insert $ sourceFilePath source
  return isObjectAvailable

isFreshCacheAvailable :: Source -> IO Bool
isFreshCacheAvailable source = do
  cachePath <- getSourceCachePath source
  isItemAvailable source cachePath

isFreshObjectAvailable :: Source -> IO Bool
isFreshObjectAvailable source = do
  objectPath <- sourceToOutputPath OutputKindObject source
  isItemAvailable source objectPath

isItemAvailable :: Source -> Path Abs File -> IO Bool
isItemAvailable source itemPath = do
  existsItem <- doesFileExist itemPath
  if not existsItem
    then return False
    else do
      srcModTime <- getModificationTime $ sourceFilePath source
      itemModTime <- getModificationTime itemPath
      return $ itemModTime > srcModTime

raiseCyclicPath :: Throw.Context -> Source -> IO a
raiseCyclicPath ctx source = do
  traceSourceList <- readIORef traceSourceListRef
  let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
  let cyclicPathList = map sourceFilePath $ reverse $ source : traceSourceList
  Throw.raiseError ctx m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

showCyclicPath :: [Path Abs File] -> T.Text
showCyclicPath pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      T.pack (toFilePath path)
    path : ps ->
      "     " <> T.pack (toFilePath path) <> showCyclicPath' ps

showCyclicPath' :: [Path Abs File] -> T.Text
showCyclicPath' pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      "\n  ~> " <> T.pack (toFilePath path)
    path : ps ->
      "\n  ~> " <> T.pack (toFilePath path) <> showCyclicPath' ps

getChildren :: Throw.Context -> Source -> IO [Source]
getChildren ctx currentSource = do
  sourceChildrenMap <- readIORef sourceChildrenMapRef
  let currentSourceFilePath = sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      let path = sourceFilePath currentSource
      (sourceList, aliasInfoList) <- run ctx (parseImportSequence ctx (sourceModule currentSource)) path
      modifyIORef' sourceChildrenMapRef $ Map.insert currentSourceFilePath sourceList
      updateSourceAliasMapRef currentSourceFilePath aliasInfoList
      return sourceList

addExtensionAlongKind :: Path Abs File -> OutputKind -> IO (Path Abs File)
addExtensionAlongKind file kind =
  case kind of
    OutputKindLLVM -> do
      addExtension ".ll" file
    OutputKindAsm -> do
      addExtension ".s" file
    OutputKindObject -> do
      addExtension ".o" file
    OutputKindExecutable -> do
      return file

type TargetString =
  String

resolveTarget :: Throw.Context -> Module -> TargetString -> IO (Path Abs File)
resolveTarget ctx mainModule target = do
  case getTargetFilePath mainModule (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      _ <- Throw.raiseError' ctx $ "no such target is defined: `" <> T.pack target <> "`"
      exitWith (ExitFailure 1)
