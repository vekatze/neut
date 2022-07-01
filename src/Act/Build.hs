module Act.Build
  ( build,
    clean,
    check,
  )
where

import Context.App
import qualified Context.LLVM as LLVM
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
import Entity.OutputKind
import Entity.Source
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

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

build :: Axis -> Maybe Target -> IO ()
build axis mTarget = do
  ensureNotInLibDir axis "build"
  case mTarget of
    Just target ->
      build' axis target
    Nothing -> do
      mainModule <- getMainModule (axis & throw)
      forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
        build' axis (T.unpack target)

build' :: Axis -> Target -> IO ()
build' axis target = do
  mainFilePath <- resolveTarget axis target
  mainSource <- getMainSource axis mainFilePath
  setMainFilePath mainFilePath
  (_, isObjectAvailable, dependenceSeq) <- computeDependence axis mainSource
  hasObjectSet <- readIORef hasObjectSetRef
  mapM_ (compile axis hasObjectSet) dependenceSeq
  unless isObjectAvailable $ link axis target $ toList dependenceSeq

check :: Axis -> Maybe FilePath -> IO ()
check axis mFilePathStr = do
  ensureNotInLibDir axis "check"
  case mFilePathStr of
    Just filePathStr -> do
      filePath <- resolveFile' filePathStr
      check' axis filePath
    Nothing -> do
      mainModule <- getMainModule (axis & throw)
      forM_ (Map.elems $ moduleTarget mainModule) $ \relPath ->
        check' axis $ getSourceDir mainModule </> relPath

check' :: Axis -> Path Abs File -> IO ()
check' axis filePath = do
  ensureFileModuleSanity axis filePath
  mainModule <- getMainModule (axis & throw)
  let source = Source {sourceModule = mainModule, sourceFilePath = filePath}
  (_, _, dependenceSeq) <- computeDependence axis source
  mapM_ (check'' axis) dependenceSeq

ensureFileModuleSanity :: Axis -> Path Abs File -> IO ()
ensureFileModuleSanity axis filePath = do
  mainModule <- getMainModule (axis & throw)
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    axis & throw & Throw.raiseError' $ "the specified file is not in the current module"

ensureNotInLibDir :: Axis -> T.Text -> IO ()
ensureNotInLibDir axis commandName = do
  mainModule <- getMainModule (axis & throw)
  libDir <- getLibraryDirPath
  when (isProperPrefixOf libDir (moduleLocation mainModule)) $
    axis & throw & Throw.raiseError' $
      "the subcommand `" <> commandName <> "` cannot be run under the library directory"

check'' :: Axis -> Source -> IO ()
check'' axis source = do
  mMainFunctionName <- getMainFunctionName axis source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain axis mainName source >>= elaborateMain axis mainName source
    Nothing ->
      void $ parseOther axis source >>= elaborateOther axis source

compile :: Axis -> S.Set (Path Abs File) -> Source -> IO ()
compile axis hasObjectSet source = do
  if S.member (sourceFilePath source) hasObjectSet
    then loadTopLevelDefinitions axis source
    else compile' axis source

loadTopLevelDefinitions :: Axis -> Source -> IO ()
loadTopLevelDefinitions axis source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint axis source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain axis mainName source >>= elaborateMain axis mainName source >>= clarifyMain axis mainName
    Nothing ->
      void $ parseOther axis source >>= elaborateOther axis source >>= clarifyOther axis

compile' :: Axis -> Source -> IO ()
compile' axis source = do
  llvmCode <- compileToLLVM axis source
  outputPath <- sourceToOutputPath OutputKindObject source
  ensureDir $ parent outputPath
  llvmOutputPath <- sourceToOutputPath OutputKindLLVM source
  L.writeFile (toFilePath llvmOutputPath) llvmCode
  (axis & llvm & LLVM.emit) OutputKindObject llvmCode outputPath

compileToLLVM :: Axis -> Source -> IO L.ByteString
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

link :: Axis -> Target -> [Source] -> IO ()
link axis target sourceList = do
  outputPath <- getExecutableOutputPath axis target
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  (axis & llvm & LLVM.link) objectPathList outputPath

clean :: Axis -> IO ()
clean axis = do
  mainModule <- getMainModule (axis & throw)
  let targetDir = getTargetDir mainModule
  b <- doesDirExist targetDir
  when b $ removeDirRecur $ getTargetDir mainModule

getExecutableOutputPath :: Axis -> Target -> IO (Path Abs File)
getExecutableOutputPath axis target = do
  mainModule <- getMainModule (axis & throw)
  resolveFile (getExecutableDir mainModule) target

sourceToOutputPath :: OutputKind -> Source -> IO (Path Abs File)
sourceToOutputPath kind source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtensionAlongKind (artifactDir </> relPathWithoutExtension) kind

getMainSource :: Axis -> Path Abs File -> IO Source
getMainSource axis mainSourceFilePath = do
  mainModule <- getMainModule (axis & throw)
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

getMainFunctionName :: Axis -> Source -> IO (Maybe T.Text)
getMainFunctionName axis source = do
  b <- isMainFile source
  if b
    then return <$> getMainFunctionName' axis source
    else return Nothing

getMainFunctionNameIfEntryPoint :: Axis -> Source -> IO (Maybe T.Text)
getMainFunctionNameIfEntryPoint axis source = do
  mainFilePath <- getMainFilePath (axis & throw)
  if sourceFilePath source == mainFilePath
    then return <$> getMainFunctionName' axis source
    else return Nothing

getMainFunctionName' :: Axis -> Source -> IO T.Text
getMainFunctionName' axis source = do
  locator <- getLocator axis source
  return $ locator <> definiteSep <> "main"

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

computeDependence :: Axis -> Source -> IO (IsCacheAvailable, IsObjectAvailable, Seq Source)
computeDependence axis source = do
  visitEnv <- readIORef visitEnvRef
  let path = sourceFilePath source
  case Map.lookup path visitEnv of
    Just VisitInfoActive ->
      raiseCyclicPath axis source
    Just VisitInfoFinish -> do
      hasCacheSet <- readIORef hasCacheSetRef
      hasObjectSet <- readIORef hasObjectSetRef
      return (path `S.member` hasCacheSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoActive
      modifyIORef' traceSourceListRef $ \sourceList -> source : sourceList
      children <- getChildren axis source
      (isCacheAvailableList, isObjectAvailableList, seqList) <- unzip3 <$> mapM (computeDependence axis) children
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

raiseCyclicPath :: Axis -> Source -> IO a
raiseCyclicPath axis source = do
  traceSourceList <- readIORef traceSourceListRef
  let m = Entity.Hint.new 1 1 $ toFilePath $ sourceFilePath source
  let cyclicPathList = map sourceFilePath $ reverse $ source : traceSourceList
  (axis & throw & Throw.raiseError) m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

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

getChildren :: Axis -> Source -> IO [Source]
getChildren axis currentSource = do
  sourceChildrenMap <- readIORef sourceChildrenMapRef
  let currentSourceFilePath = sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      let path = sourceFilePath currentSource
      -- initializeParserForFile $ sourceFilePath currentSource
      -- skip
      (sourceList, aliasInfoList) <- run (parseImportSequence axis (sourceModule currentSource)) path
      -- (sourceList, aliasInfoList) <- parseImportSequence $ sourceModule currentSource
      modifyIORef' sourceChildrenMapRef $ Map.insert currentSourceFilePath sourceList
      updateSourceAliasMapRef currentSourceFilePath aliasInfoList
      -- modifyIORef' sourceAliasMapRef $ Map.insert currentSourceFilePath aliasInfoList
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

type Target =
  String

resolveTarget :: Axis -> Target -> IO (Path Abs File)
resolveTarget axis target = do
  mainModule <- getMainModule (axis & throw)
  case getTargetFilePath mainModule (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      -- l <-
      _ <- axis & throw & Throw.raiseError' $ "no such target is defined: `" <> T.pack target <> "`"
      -- outputLog l
      exitWith (ExitFailure 1)
