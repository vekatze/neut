{-# LANGUAGE TemplateHaskell #-}

module Command.Build
  ( build,
    clean,
    check,
    OutputKind (..),
  )
where

import Clarify (clarifyMain, clarifyOther)
import Control.Monad (forM_, unless, void, when)
import Data.Basic (OutputKind (..), newHint)
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)
import Data.Global (getLibraryDirPath, getMainFilePath, modifiedSourceSetRef, nsSep, outputLog, setMainFilePath, sourceAliasMapRef)
import qualified Data.HashMap.Lazy as Map
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List (foldl')
import Data.Log (raiseError, raiseError')
import Data.Maybe (fromMaybe)
import Data.Module (Module (moduleLocation, moduleTarget), getArtifactDir, getExecutableDir, getMainModule, getSourceDir, getTargetDir, getTargetFilePath)
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import qualified Data.Set as S
import Data.Source (Source (Source, sourceModule), getRelPathFromSourceDir, getSection, getSourceCachePath, isMainFile, sourceFilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Elaborate (elaborateMain, elaborateOther)
import Emit (emitMain, emitOther)
import GHC.IO.Handle (hClose)
import Lower (lowerMain, lowerOther)
import Parse (parseMain, parseOther)
import Parse.Core (initializeParserForFile, skip)
-- import Parse.Enum (initializeEnumEnv)
import Parse.Import (parseImportSequence)
import Path
  ( Abs,
    File,
    Path,
    addExtension,
    isProperPrefixOf,
    parent,
    splitExtension,
    toFilePath,
    (</>),
  )
import Path.IO (doesDirExist, doesFileExist, ensureDir, getModificationTime, removeDirRecur, resolveFile, resolveFile')
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
  ( CreateProcess (std_in),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
    withCreateProcess,
  )
import System.Process.Internals (std_err)

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

type IsModified =
  Bool

type ClangOption =
  String

build :: Maybe Target -> Maybe ClangOption -> IO ()
build mTarget mClangOptStr = do
  ensureNotInLibDir "build"
  case mTarget of
    Just target ->
      build' target mClangOptStr
    Nothing -> do
      mainModule <- getMainModule
      forM_ (Map.keys $ moduleTarget mainModule) $ \target ->
        build' (T.unpack target) mClangOptStr

build' :: Target -> Maybe ClangOption -> IO ()
build' target mClangOptStr = do
  mainFilePath <- resolveTarget target
  mainSource <- getMainSource mainFilePath
  setMainFilePath mainFilePath
  -- initializeEnumEnv
  (isModified, dependenceSeq) <- computeDependence mainSource
  modifiedSourceSet <- readIORef modifiedSourceSetRef
  let clangOptStr = fromMaybe "" mClangOptStr
  mapM_ (compile modifiedSourceSet clangOptStr) dependenceSeq
  when isModified $ link target mClangOptStr $ toList dependenceSeq

check :: Maybe FilePath -> IO ()
check mFilePathStr = do
  ensureNotInLibDir "check"
  case mFilePathStr of
    Just filePathStr -> do
      filePath <- resolveFile' filePathStr
      check' filePath
    Nothing -> do
      mainModule <- getMainModule
      forM_ (Map.elems $ moduleTarget mainModule) $ \relPath ->
        check' $ getSourceDir mainModule </> relPath

check' :: Path Abs File -> IO ()
check' filePath = do
  ensureFileModuleSanity filePath
  mainModule <- getMainModule
  let source = Source {sourceModule = mainModule, sourceFilePath = filePath}
  -- initializeEnumEnv
  (_, dependenceSeq) <- computeDependence source
  mapM_ check'' dependenceSeq

ensureFileModuleSanity :: Path Abs File -> IO ()
ensureFileModuleSanity filePath = do
  mainModule <- getMainModule
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    raiseError' "the specified file is not in the current module"

ensureNotInLibDir :: T.Text -> IO ()
ensureNotInLibDir commandName = do
  mainModule <- getMainModule
  libDir <- getLibraryDirPath
  when (isProperPrefixOf libDir (moduleLocation mainModule)) $
    raiseError' $ "the subcommand `" <> commandName <> "` cannot be run under the library directory"

check'' :: Source -> IO ()
check'' source = do
  mMainFunctionName <- getMainFunctionName source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain mainName source >>= elaborateMain mainName source
    Nothing ->
      void $ parseOther source >>= elaborateOther source

compile :: S.Set (Path Abs File) -> String -> Source -> IO ()
compile modifiedSourceSet clangOptStr source = do
  if S.member (sourceFilePath source) modifiedSourceSet
    then compile' clangOptStr source
    else loadTopLevelDefinitions source

loadTopLevelDefinitions :: Source -> IO ()
loadTopLevelDefinitions source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint source
  case mMainFunctionName of
    Just mainName ->
      void $ parseMain mainName source >>= elaborateMain mainName source >>= clarifyMain mainName
    Nothing ->
      void $ parseOther source >>= elaborateOther source >>= clarifyOther

compile' :: String -> Source -> IO ()
compile' clangOptStr source = do
  llvm <- compileToLLVM source
  outputPath <- sourceToOutputPath OutputKindObject source
  ensureDir $ parent outputPath
  llvmOutputPath <- sourceToOutputPath OutputKindLLVM source
  L.writeFile (toFilePath llvmOutputPath) llvm
  let clangCmd = proc "clang" $ clangOptWith OutputKindLLVM outputPath ++ ["-c"] ++ words clangOptStr
  withCreateProcess clangCmd {std_in = CreatePipe, std_err = CreatePipe} $ \(Just stdin) _ (Just clangErrorHandler) clangProcessHandler -> do
    L.hPut stdin llvm
    hClose stdin
    clangExitCode <- waitForProcess clangProcessHandler
    raiseIfFailure "clang" clangExitCode clangErrorHandler
    return ()

compileToLLVM :: Source -> IO L.ByteString
compileToLLVM source = do
  mMainFunctionName <- getMainFunctionNameIfEntryPoint source
  case mMainFunctionName of
    Just mainName ->
      parseMain mainName source >>= elaborateMain mainName source >>= clarifyMain mainName >>= lowerMain >>= emitMain
    Nothing ->
      parseOther source >>= elaborateOther source >>= clarifyOther >>= lowerOther >> emitOther

link :: Target -> Maybe String -> [Source] -> IO ()
link target mClangOptStr sourceList = do
  outputPath <- getExecutableOutputPath target
  ensureDir $ parent outputPath
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  let clangCmd = proc "clang" $ clangLinkOpt objectPathList outputPath $ fromMaybe "" mClangOptStr
  (_, _, Just clangErrorHandler, clangHandler) <-
    createProcess clangCmd {std_err = CreatePipe}
  clangExitCode <- waitForProcess clangHandler
  raiseIfFailure "clang" clangExitCode clangErrorHandler

clean :: IO ()
clean = do
  mainModule <- getMainModule
  let targetDir = getTargetDir mainModule
  b <- doesDirExist targetDir
  when b $ removeDirRecur $ getTargetDir mainModule

getExecutableOutputPath :: Target -> IO (Path Abs File)
getExecutableOutputPath target = do
  mainModule <- getMainModule
  resolveFile (getExecutableDir mainModule) target

sourceToOutputPath :: OutputKind -> Source -> IO (Path Abs File)
sourceToOutputPath kind source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

getMainSource :: Path Abs File -> IO Source
getMainSource mainSourceFilePath = do
  mainModule <- getMainModule
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

getMainFunctionName :: Source -> IO (Maybe T.Text)
getMainFunctionName source = do
  b <- isMainFile source
  if b
    then return <$> getMainFunctionName' source
    else return Nothing

getMainFunctionNameIfEntryPoint :: Source -> IO (Maybe T.Text)
getMainFunctionNameIfEntryPoint source = do
  mainFilePath <- getMainFilePath
  if sourceFilePath source == mainFilePath
    then return <$> getMainFunctionName' source
    else return Nothing

getMainFunctionName' :: Source -> IO T.Text
getMainFunctionName' source = do
  section <- getSection source
  return $ section <> nsSep <> "main"

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

computeDependence :: Source -> IO (IsModified, Seq Source)
computeDependence source = do
  visitEnv <- readIORef visitEnvRef
  case Map.lookup (sourceFilePath source) visitEnv of
    Just VisitInfoActive ->
      raiseCyclicPath source
    Just VisitInfoFinish ->
      return (False, Seq.empty)
    Nothing -> do
      let path = sourceFilePath source
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoActive
      modifyIORef' traceSourceListRef $ \sourceList -> source : sourceList
      children <- getChildren source
      (isModifiedList, seqList) <- unzip <$> mapM computeDependence children
      modifyIORef' traceSourceListRef tail
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoFinish
      isModified <- computeModificationStatus isModifiedList source
      return (isModified, foldl' (><) Seq.empty seqList |> source)

computeModificationStatus :: [IsModified] -> Source -> IO IsModified
computeModificationStatus isModifiedList source = do
  b <- isSourceModified source
  let isModified = or $ b : isModifiedList
  when isModified $
    modifyIORef' modifiedSourceSetRef $ S.insert $ sourceFilePath source
  return isModified

isSourceModified :: Source -> IO Bool
isSourceModified source = do
  objectPath <- sourceToOutputPath OutputKindObject source
  cachePath <- getSourceCachePath source
  b1 <- isSourceModified' source objectPath
  b2 <- isSourceModified' source cachePath
  return $ b1 || b2

isSourceModified' :: Source -> Path Abs File -> IO Bool
isSourceModified' source itemPath = do
  existsItem <- doesFileExist itemPath
  if not existsItem
    then return True
    else do
      srcModTime <- getModificationTime $ sourceFilePath source
      itemModTime <- getModificationTime itemPath
      return $ itemModTime < srcModTime

raiseCyclicPath :: Source -> IO a
raiseCyclicPath source = do
  traceSourceList <- readIORef traceSourceListRef
  let m = newHint 1 1 $ toFilePath $ sourceFilePath source
  let cyclicPathList = map sourceFilePath $ reverse $ source : traceSourceList
  raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

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

getChildren :: Source -> IO [Source]
getChildren currentSource = do
  sourceChildrenMap <- readIORef sourceChildrenMapRef
  let currentSourceFilePath = sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      initializeParserForFile $ sourceFilePath currentSource
      skip
      (sourceList, aliasInfoList) <- parseImportSequence $ sourceModule currentSource
      modifyIORef' sourceChildrenMapRef $ Map.insert currentSourceFilePath sourceList
      modifyIORef' sourceAliasMapRef $ Map.insert currentSourceFilePath aliasInfoList
      return sourceList

attachExtension :: Path Abs File -> OutputKind -> IO (Path Abs File)
attachExtension file kind =
  case kind of
    OutputKindLLVM -> do
      addExtension ".ll" file
    OutputKindAsm -> do
      addExtension ".s" file
    OutputKindObject -> do
      addExtension ".o" file
    OutputKindExecutable -> do
      return file

clangLinkOpt :: [Path Abs File] -> Path Abs File -> String -> [String]
clangLinkOpt objectPathList outputPath additionalOptionStr = do
  let pathList = map toFilePath objectPathList
  ["-Wno-override-module", "-O2", "-o", toFilePath outputPath] ++ pathList ++ words additionalOptionStr

clangOptWith :: OutputKind -> Path Abs File -> [String]
clangOptWith kind outputPath =
  case kind of
    OutputKindAsm ->
      "-S" : clangBaseOpt outputPath
    _ ->
      clangBaseOpt outputPath

clangBaseOpt :: Path Abs File -> [String]
clangBaseOpt outputPath =
  [ "-xir",
    "-Wno-override-module",
    "-O2",
    "-",
    "-o",
    toFilePath outputPath
  ]

type Target =
  String

resolveTarget :: Target -> IO (Path Abs File)
resolveTarget target = do
  mainModule <- getMainModule
  case getTargetFilePath mainModule (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      l <- raiseError' $ "no such target is defined: `" <> T.pack target <> "`"
      outputLog l
      exitWith (ExitFailure 1)

raiseIfFailure :: T.Text -> ExitCode -> Handle -> IO ()
raiseIfFailure procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      raiseError' $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
