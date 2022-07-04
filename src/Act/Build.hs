module Act.Build
  ( build,
    clean,
    check,
    BuildConfig (..),
    CheckConfig (..),
    CleanConfig (..),
  )
where

import qualified Context.App as App
import qualified Context.Enum as Enum
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
import Data.Function
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
import Entity.Module.Reflect
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
    initializeMainModule throwCtx
    ensureNotInLibDir throwCtx "build"
    case mTarget cfg of
      Just target ->
        build' mode throwCtx logCtx (shouldCancelAlloc cfg) target
      Nothing -> do
        mainModule <- getMainModule throwCtx
        forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
          build' mode throwCtx logCtx (shouldCancelAlloc cfg) (T.unpack target)

build' :: Mode.Mode -> Throw.Context -> Log.Context -> Bool -> TargetString -> IO ()
build' mode throwCtx logCtx cancelAllocFlag target = do
  mainFilePath <- resolveTarget throwCtx target
  mainSource <- getMainSource throwCtx mainFilePath
  ctx <- newCtx mode throwCtx logCtx cancelAllocFlag mainSource
  (_, isObjectAvailable, dependenceSeq) <- computeDependence throwCtx mainSource
  hasObjectSet <- readIORef hasObjectSetRef
  mapM_ (compile ctx hasObjectSet) dependenceSeq
  unless isObjectAvailable $ link ctx target $ toList dependenceSeq

newCtx :: Mode.Mode -> Throw.Context -> Log.Context -> Bool -> Source -> IO App.Axis
newCtx mode throwCtx logCtx cancelAllocFlag source = do
  globalLocator <- getGlobalLocator throwCtx source
  gensymCtx <- Mode.gensymCtx mode $ Gensym.Config {}
  enumCtx <- Mode.enumCtx mode $ Enum.Config {Enum.throwCtx = throwCtx}
  llvmCtx <- Mode.llvmCtx mode $ LLVM.Config {LLVM.throwCtx = throwCtx, LLVM.clangOptString = ""} -- fixme
  globalCtx <- Mode.globalCtx mode $ Global.Config {Global.throwCtx = throwCtx}
  locatorCtx <-
    Mode.locatorCtx mode $
      Locator.Config
        { Locator.currentGlobalLocator = globalLocator,
          Locator.currentLocalLocator = [],
          Locator.throwCtx = throwCtx
        }
  return $
    App.Axis
      { App.log = logCtx,
        App.throw = throwCtx,
        App.gensym = gensymCtx,
        App.enum = enumCtx,
        App.llvm = llvmCtx,
        App.global = globalCtx,
        App.locator = locatorCtx,
        App.shouldCancelAlloc = cancelAllocFlag,
        App.initialSource = source,
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
    initializeMainModule throwCtx
    ensureNotInLibDir throwCtx "check"
    case mFilePathString cfg of
      Just filePathStr -> do
        filePath <- resolveFile' filePathStr
        check' mode throwCtx logCtx filePath
      Nothing -> do
        mainModule <- getMainModule throwCtx
        forM_ (Map.elems $ moduleTarget mainModule) $ \relPath ->
          check' mode throwCtx logCtx $ getSourceDir mainModule </> relPath

check' :: Mode.Mode -> Throw.Context -> Log.Context -> Path Abs File -> IO ()
check' mode throwCtx logCtx filePath = do
  ensureFileModuleSanity throwCtx filePath
  mainModule <- getMainModule throwCtx
  let source = Source {sourceModule = mainModule, sourceFilePath = filePath}
  ctx <- newCtx mode throwCtx logCtx False source
  (_, _, dependenceSeq) <- computeDependence throwCtx source
  mapM_ (check'' ctx) dependenceSeq

ensureFileModuleSanity :: Throw.Context -> Path Abs File -> IO ()
ensureFileModuleSanity ctx filePath = do
  mainModule <- getMainModule ctx
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' ctx "the specified file is not in the current module"

ensureNotInLibDir :: Throw.Context -> T.Text -> IO ()
ensureNotInLibDir ctx commandName = do
  mainModule <- getMainModule ctx
  libDir <- getLibraryDirPath
  when (isProperPrefixOf libDir (moduleLocation mainModule)) $
    Throw.raiseError' ctx $
      "the subcommand `" <> commandName <> "` cannot be run under the library directory"

check'' :: App.Axis -> Source -> IO ()
check'' axis source = do
  mMainFunctionName <- getMainFunctionName axis source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain axis mainName source >>= elaborateMain axis mainName source
    Nothing ->
      void $ parseOther axis source >>= elaborateOther axis source

compile :: App.Axis -> S.Set (Path Abs File) -> Source -> IO ()
compile axis hasObjectSet source = do
  if S.member (sourceFilePath source) hasObjectSet
    then loadTopLevelDefinitions axis source
    else compile' axis source

loadTopLevelDefinitions :: App.Axis -> Source -> IO ()
loadTopLevelDefinitions axis source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint axis source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain axis mainName source >>= elaborateMain axis mainName source >>= clarifyMain axis mainName
    Nothing ->
      void $ parseOther axis source >>= elaborateOther axis source >>= clarifyOther axis

compile' :: App.Axis -> Source -> IO ()
compile' axis source = do
  llvmCode <- compileToLLVM axis source
  outputPath <- sourceToOutputPath OutputKindObject source
  ensureDir $ parent outputPath
  llvmOutputPath <- sourceToOutputPath OutputKindLLVM source
  L.writeFile (toFilePath llvmOutputPath) llvmCode
  (axis & App.llvm & LLVM.emit) OutputKindObject llvmCode outputPath

compileToLLVM :: App.Axis -> Source -> IO L.ByteString
compileToLLVM axis source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint axis source
  case mMainFunctionName of
    Just mainName ->
      parseMain axis mainName source
        >>= elaborateMain axis mainName source
        >>= clarifyMain axis mainName
        >>= lowerMain axis
        >>= emitMain axis
    Nothing ->
      parseOther axis source
        >>= elaborateOther axis source
        >>= clarifyOther axis
        >>= lowerOther axis
        >> emitOther axis

link :: App.Axis -> TargetString -> [Source] -> IO ()
link axis target sourceList = do
  outputPath <- getExecutableOutputPath axis target
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  (axis & App.llvm & LLVM.link) objectPathList outputPath

data CleanConfig = CleanConfig
  { cleanLogCfg :: Log.Config,
    cleanThrowCfg :: Throw.Config
  }

clean :: Mode.Mode -> CleanConfig -> IO ()
clean mode cfg = do
  throwCtx <- Mode.throwCtx mode $ cleanThrowCfg cfg
  logCtx <- Mode.logCtx mode $ cleanLogCfg cfg
  Throw.run throwCtx (Log.printLog logCtx) $ do
    initializeMainModule throwCtx
    mainModule <- getMainModule throwCtx
    let targetDir = getTargetDir mainModule
    b <- doesDirExist targetDir
    when b $ removeDirRecur $ getTargetDir mainModule

getExecutableOutputPath :: App.Axis -> TargetString -> IO (Path Abs File)
getExecutableOutputPath axis target = do
  mainModule <- getMainModule (axis & App.throw)
  resolveFile (getExecutableDir mainModule) target

sourceToOutputPath :: OutputKind -> Source -> IO (Path Abs File)
sourceToOutputPath kind source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtensionAlongKind (artifactDir </> relPathWithoutExtension) kind

getMainSource :: Throw.Context -> Path Abs File -> IO Source
getMainSource axis mainSourceFilePath = do
  mainModule <- getMainModule axis
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

getMainFunctionName :: App.Axis -> Source -> IO (Maybe T.Text)
getMainFunctionName axis source = do
  b <- isMainFile source
  if b
    then return <$> getMainFunctionName' axis source
    else return Nothing

getMainFunctionNameIfEntryPoint :: App.Axis -> Source -> IO (Maybe T.Text)
getMainFunctionNameIfEntryPoint axis source = do
  if sourceFilePath source == sourceFilePath (App.initialSource axis)
    then return <$> getMainFunctionName' axis source
    else return Nothing

getMainFunctionName' :: App.Axis -> Source -> IO T.Text
getMainFunctionName' axis source = do
  globalLocator <- getGlobalLocator (axis & App.throw) source
  return $ globalLocator <> definiteSep <> "main"

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

resolveTarget :: Throw.Context -> TargetString -> IO (Path Abs File)
resolveTarget axis target = do
  mainModule <- getMainModule axis
  case getTargetFilePath mainModule (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      -- l <-
      _ <- Throw.raiseError' axis $ "no such target is defined: `" <> T.pack target <> "`"
      -- outputLog l
      exitWith (ExitFailure 1)
