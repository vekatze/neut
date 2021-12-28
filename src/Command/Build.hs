{-# LANGUAGE TemplateHaskell #-}

module Command.Build
  ( build,
    clean,
    OutputKind (..),
  )
where

import Clarify (clarifyMain, clarifyOther)
import Data.Basic (newHint)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)
import Data.Global (getMainFilePath, nsSep, outputLog, setMainFilePath, shouldColorize, sourceAliasMapRef)
import qualified Data.HashMap.Lazy as Map
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.List (foldl')
import Data.Log (raiseError, raiseError')
import Data.Module (getArtifactDir, getExecutableDir, getMainModule, getTargetDir, getTargetFilePath)
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import Data.Source (Source (Source, sourceModule), getRelPathFromSourceDir, getSection, sourceFilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Elaborate (elaborateMain, elaborateOther)
import Emit (emitMain, emitOther)
import GHC.IO.Handle (hClose)
import Lower (lowerMain, lowerOther)
import Parse (parseMain, parseOther)
import Parse.Core (initializeParserForFile, skip)
import Parse.Enum (initializeEnumEnv)
import Parse.Import (parseImportSequence)
import Path
  ( Abs,
    File,
    Path,
    addExtension,
    parent,
    splitExtension,
    toFilePath,
    (</>),
  )
import Path.IO (ensureDir, removeDirRecur, resolveFile)
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

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  | OutputKindExecutable
  | OutputKindAsm
  deriving (Show)

data VisitInfo
  = VisitInfoActive
  | VisitInfoFinish

type ShouldColorize =
  Bool

type ClangOption =
  String

build :: Target -> OutputKind -> ShouldColorize -> Maybe ClangOption -> IO ()
build target outputKind colorizeFlag mClangOptStr = do
  writeIORef shouldColorize colorizeFlag
  mainFilePath <- resolveTarget target
  mainSource <- getMainSource mainFilePath
  setMainFilePath mainFilePath
  initializeEnumEnv
  dependenceSeq <- computeDependence mainSource
  mapM_ compile (toList dependenceSeq)
  link target $ toList dependenceSeq

compile :: Source -> IO ()
compile source = do
  llvm <- sourceToLLVM source
  outputPath <- sourceToOutputPath OutputKindObject source
  ensureDir $ parent outputPath
  llvmOutputPath <- sourceToOutputPath OutputKindLLVM source
  L.writeFile (toFilePath llvmOutputPath) $ toLazyByteString llvm
  let clangCmd = proc "clang" $ clangOptWith OutputKindLLVM outputPath ++ ["-c"]
  let llvmIR = toLazyByteString llvm
  withCreateProcess clangCmd {std_in = CreatePipe, std_err = CreatePipe} $ \(Just stdin) _ (Just clangErrorHandler) clangProcessHandler -> do
    L.hPut stdin llvmIR
    hClose stdin
    clangExitCode <- waitForProcess clangProcessHandler
    raiseIfFailure "clang" clangExitCode clangErrorHandler
    return ()

link :: Target -> [Source] -> IO ()
link target sourceList = do
  outputPath <- getExecutableOutputPath target
  ensureDir $ parent outputPath
  objectPathList <- mapM (sourceToOutputPath OutputKindObject) sourceList
  let clangCmd = proc "clang" $ clangLinkOpt objectPathList outputPath
  (_, _, Just clangErrorHandler, clangHandler) <-
    createProcess clangCmd {std_err = CreatePipe}
  clangExitCode <- waitForProcess clangHandler
  raiseIfFailure "clang" clangExitCode clangErrorHandler

sourceToLLVM :: Source -> IO Builder
sourceToLLVM source = do
  mMainFunctionName <- getMainFunctionName source
  case mMainFunctionName of
    Just mainName ->
      parseMain mainName source >>= elaborateMain mainName source >>= clarifyMain mainName >>= lowerMain >>= emitMain
    Nothing ->
      parseOther source >>= elaborateOther source >>= clarifyOther >>= lowerOther >> emitOther

clean :: IO ()
clean = do
  mainModule <- getMainModule
  removeDirRecur $ getTargetDir mainModule

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
  mainFilePath <- getMainFilePath
  if sourceFilePath source /= mainFilePath
    then return Nothing
    else do
      section <- getSection source
      return $ Just $ T.intercalate nsSep $ section ++ ["main"]

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

computeDependence :: Source -> IO (Seq Source)
computeDependence source = do
  visitEnv <- readIORef visitEnvRef
  case Map.lookup (sourceFilePath source) visitEnv of
    Just VisitInfoActive -> do
      traceSourceList <- readIORef traceSourceListRef
      let m = newHint 1 1 $ toFilePath $ sourceFilePath source
      let cyclicPathList = map sourceFilePath $ reverse $ source : traceSourceList
      raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList
    Just VisitInfoFinish ->
      return Seq.empty
    Nothing -> do
      children <- getChildren source
      let path = sourceFilePath source
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoActive
      modifyIORef' traceSourceListRef $ \sourceList -> source : sourceList
      dependenceSeq <- foldl' (><) Seq.empty <$> mapM computeDependence children
      modifyIORef' traceSourceListRef tail
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoFinish
      return $ dependenceSeq |> source

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

instance Read OutputKind where
  readsPrec _ "object" =
    [(OutputKindObject, [])]
  readsPrec _ "llvm" =
    [(OutputKindLLVM, [])]
  readsPrec _ "asm" =
    [(OutputKindAsm, [])]
  readsPrec _ _ =
    []

clangLinkOpt :: [Path Abs File] -> Path Abs File -> [String]
clangLinkOpt objectPathList outputPath = do
  let pathList = map toFilePath objectPathList
  ["-Wno-override-module", "-O2", "-o", toFilePath outputPath] ++ pathList

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