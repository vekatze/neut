{-# LANGUAGE TemplateHaskell #-}

module Command.Build
  ( build,
    clean,
    OutputKind (..),
  )
where

import Clarify (clarify)
import Control.Monad (forM_, (>=>))
import Data.Basic (Hint)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Foldable (toList)
import Data.Global (VisitInfo (..), mainModuleDirRef, outputLog, setCurrentFilePath, shouldColorize)
import qualified Data.HashMap.Lazy as Map
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.List (foldl')
import Data.Log (raiseError')
import Data.Maybe (fromMaybe)
import Data.Module (Source (Source, sourceModule), getMainModule, getModuleArtifactDir, getModuleRootDir, getRelPathFromSourceDir, sourceFilePath)
import Data.Sequence as Seq
  ( Seq,
    ViewL (EmptyL, (:<)),
    empty,
    viewl,
    (><),
    (|>),
  )
import Data.Spec (Spec, getEntryPoint, getTargetDir)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Elaborate (elaborate)
import Emit (emit)
import GHC.IO.Handle (hClose)
import Lower (lower)
import Parse (parse)
import Parse.Core (currentHint, initializeState, skip)
import Parse.Enum (initializeEnumEnv)
import Parse.Import (Signature, parseImportSequence, signatureToSource)
import Parse.Spec (getMainSpec, moduleToSpec)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    addExtension,
    mkRelDir,
    parent,
    parseRelFile,
    splitExtension,
    toFilePath,
    (</>),
  )
import Path.IO (ensureDir, removeDirRecur)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
  ( CreateProcess (std_in),
    StdStream (CreatePipe),
    proc,
    waitForProcess,
    withCreateProcess,
  )

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  | OutputKindExecutable
  | OutputKindAsm
  deriving (Show)

type ShouldColorize =
  Bool

type ClangOption =
  String

build :: Target -> OutputKind -> ShouldColorize -> Maybe ClangOption -> IO ()
build target outputKind colorizeFlag mClangOptStr = do
  writeIORef shouldColorize colorizeFlag
  mainSpec <- getMainSpec
  entryFilePath <- resolveTarget target mainSpec
  mainSource <- getMainSource entryFilePath
  writeIORef mainModuleDirRef $ getModuleRootDir $ sourceModule mainSource
  initializeEnumEnv
  dependenceSeq <- computeDependence mainSource
  forM_ (toList dependenceSeq) $ \source -> do
    compile source
  -- link dependenceSeq
  _ <- error "stop"
  outputPath <- getOutputPath target mainSpec outputKind
  case outputKind of
    OutputKindLLVM -> do
      llvmIRBuilder <- sourceToLLVM mainSource
      let llvmIR = toLazyByteString llvmIRBuilder
      L.writeFile (toFilePath outputPath) llvmIR
    OutputKindAsm ->
      undefined
    OutputKindObject -> do
      let additionalClangOptions = words $ fromMaybe "" mClangOptStr
      let clangCmd = proc "clang" $ clangOptWith outputKind outputPath ++ additionalClangOptions ++ ["-c"]
      llvmIRBuilder <- sourceToLLVM mainSource
      let llvmIR = toLazyByteString llvmIRBuilder
      withCreateProcess clangCmd {std_in = CreatePipe} $ \(Just stdin) _ _ clangProcessHandler -> do
        L.hPut stdin llvmIR
        hClose stdin
        exitCode <- waitForProcess clangProcessHandler
        exitWith exitCode
    OutputKindExecutable ->
      undefined

compile :: Source -> IO ()
compile source = do
  llvm <- sourceToLLVM source
  outputPath <- sourceToOutputPath source OutputKindObject
  ensureDir $ parent outputPath
  let clangCmd = proc "clang" $ clangOptWith OutputKindLLVM outputPath ++ ["-c"]
  let llvmIR = toLazyByteString llvm
  withCreateProcess clangCmd {std_in = CreatePipe} $ \(Just stdin) _ _ clangProcessHandler -> do
    L.hPut stdin llvmIR
    hClose stdin
    _ <- waitForProcess clangProcessHandler
    return ()

clean :: IO ()
clean = do
  mainSpec <- getMainSpec
  removeDirRecur $ getTargetDir mainSpec

sourceToOutputPath :: Source -> OutputKind -> IO (Path Abs File)
sourceToOutputPath source kind = do
  let artifactDir = getModuleArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

-- undefined

-- exitWith exitCode

link :: Seq Source -> IO ()
link = undefined

sourceToLLVM :: Source -> IO Builder
sourceToLLVM =
  parse >=> elaborate >=> clarify >=> lower >=> emit

-- ensureMain :: IO ()
-- ensureMain = do
--   flag <- readIORef isMain
--   when flag $ do
--     m <- currentHint
--     _ <- discern $ m :< WeakTermVar (asIdent "main")
--     return ()

getMainSource :: Path Abs File -> IO Source
getMainSource mainSourceFilePath = do
  mainModule <- getMainModule
  return $
    Source
      { sourceModule = mainModule,
        sourceFilePath = mainSourceFilePath
      }

{-# NOINLINE visitEnvRef #-}
visitEnvRef :: IORef (Map.HashMap (Path Abs File) VisitInfo)
visitEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE sourceChildrenMapRef #-}
sourceChildrenMapRef :: IORef (Map.HashMap (Path Abs File) [Source])
sourceChildrenMapRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE sourceAliasMapRef #-}
sourceAliasMapRef :: IORef (Map.HashMap (Path Abs File) [(Signature, Maybe T.Text)])
sourceAliasMapRef =
  unsafePerformIO (newIORef Map.empty)

computeDependence :: Source -> IO (Seq Source)
computeDependence source = do
  visitEnv <- readIORef visitEnvRef
  case Map.lookup (sourceFilePath source) visitEnv of
    Just VisitInfoActive ->
      error "cyclic inclusion"
    Just VisitInfoFinish ->
      return Seq.empty
    Nothing -> do
      children <- getChildren source
      let path = sourceFilePath source
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoActive
      dependenceSeq <- foldl' (><) Seq.empty <$> mapM computeDependence children
      modifyIORef' visitEnvRef $ Map.insert path VisitInfoFinish
      return $ dependenceSeq |> source

showCyclicPath :: [Path Abs File] -> T.Text
showCyclicPath pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      T.pack (toFilePath path)
    (path : ps) ->
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

-- raiseCyclicInclusion :: Hint -> Path Abs File -> IO b
-- raiseCyclicInclusion m newPath = do
--   tenv <- readIORef traceEnv
--   let cyclicPath = dropWhile (/= newPath) (reverse tenv) ++ [newPath]
--   raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPath

getChildren :: Source -> IO [Source]
getChildren currentSource = do
  sourceChildrenMap <- readIORef sourceChildrenMapRef
  case Map.lookup (sourceFilePath currentSource) sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      setCurrentFilePath $ sourceFilePath currentSource
      TIO.readFile (toFilePath (sourceFilePath currentSource)) >>= initializeState
      skip
      m <- currentHint
      importSequence <- parseImportSequence
      let signatureList = map fst importSequence
      currentSpec <- sourceToSpec m currentSource
      sourceList <- mapM (signatureToSource currentSpec) signatureList
      modifyIORef' sourceChildrenMapRef $ Map.insert (sourceFilePath currentSource) sourceList
      modifyIORef' sourceAliasMapRef $ Map.insert (sourceFilePath currentSource) importSequence
      return sourceList

-- で、リストがemptyになるまでCPUをわりあてつづけて（プロセスが終わるのをwaitしながら）、
-- emptyになった時点ですべて処理が終了したことになる。みたいな。
-- ここでCPUのわりあては、ようはprocessのうち終了してないものの数がn以下になるようにする、という形でおこなわれる。

-- mapM (signatureToSource currentSpec . fst) importSequence

sourceToSpec :: Hint -> Source -> IO Spec
sourceToSpec m source = do
  moduleToSpec m (sourceModule source)

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

getTargetDirName :: OutputKind -> Path Rel Dir
getTargetDirName kind =
  case kind of
    OutputKindLLVM ->
      $(mkRelDir "llvm")
    OutputKindAsm ->
      $(mkRelDir "assembly")
    OutputKindObject ->
      $(mkRelDir "object")
    OutputKindExecutable ->
      $(mkRelDir "executable")

instance Read OutputKind where
  readsPrec _ "object" =
    [(OutputKindObject, [])]
  readsPrec _ "llvm" =
    [(OutputKindLLVM, [])]
  readsPrec _ "asm" =
    [(OutputKindAsm, [])]
  readsPrec _ _ =
    []

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

-- clangLibOpt :: Path Abs File -> [String]
-- clangLibOpt outputPath =
--   [ "-Wno-override-module",
--     "-O2",
--     "-o",
--     toFilePath outputPath
--   ]

type Target =
  String

resolveTarget :: Target -> Spec -> IO (Path Abs File)
resolveTarget target mainSpec = do
  case getEntryPoint mainSpec (T.pack target) of
    Just path ->
      return path
    Nothing -> do
      l <- raiseError' $ "no such target is defined: `" <> T.pack target <> "`"
      outputLog l
      exitWith (ExitFailure 1)

getOutputPath :: Target -> Spec -> OutputKind -> IO (Path Abs File)
getOutputPath target mainSpec kind = do
  targetFile <- parseRelFile target
  let targetDir = getTargetDir mainSpec
  ensureDir targetDir
  attachExtension (targetDir </> getTargetDirName kind </> targetFile) kind
