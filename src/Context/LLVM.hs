module Context.LLVM
  ( emit,
    link,
    ensureSetupSanity,
    getClangOptString,
    setClangOptString,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.External qualified as External
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.Config.Build
import Entity.Const
import Entity.OutputKind qualified as OK
import GHC.IO.Handle
import Path
import Path.IO
import System.Environment
import System.Exit
import System.Process

type ClangOption = String

type LLVMCode = L.ByteString

ensureSetupSanity :: Config -> App ()
ensureSetupSanity cfg = do
  let willBuildObjects = OK.Object `elem` outputKindList cfg
  let willLink = not $ shouldSkipLink cfg
  when (not willBuildObjects && willLink) $
    Throw.raiseError' "`--skip-link` must be set explicitly when `--emit` doesn't contain `object`"

emit :: [OK.OutputKind] -> L.ByteString -> App ()
emit outputKindList llvmCode = do
  source <- Env.getCurrentSource
  kindPathList <- zipWithM Path.attachOutputPath outputKindList (repeat source)
  forM_ kindPathList $ \(_, outputPath) -> Path.ensureDir $ parent outputPath
  emitAll llvmCode kindPathList

emitAll :: LLVMCode -> [(OK.OutputKind, Path Abs File)] -> App ()
emitAll llvmCode kindPathList = do
  case kindPathList of
    [] ->
      return ()
    (kind, path) : rest -> do
      emit' llvmCode kind path
      emitAll llvmCode rest

emit' :: LLVMCode -> OK.OutputKind -> Path Abs File -> App ()
emit' llvmCode kind path = do
  clangOptString <- getClangOptString
  case kind of
    OK.LLVM -> do
      Path.writeByteString path llvmCode
    OK.Asm ->
      emitInner ("-S" : words clangOptString) llvmCode path
    OK.Object ->
      emitInner (words clangOptString) llvmCode path

emitInner :: [ClangOption] -> L.ByteString -> Path Abs File -> App ()
emitInner additionalClangOptions llvm outputPath = do
  clang <- liftIO getClang
  let clangCmd = proc clang $ clangBaseOpt outputPath ++ additionalClangOptions
  withRunInIO $ \runInIO ->
    withCreateProcess clangCmd {std_in = CreatePipe, std_err = CreatePipe} $
      \mStdin _ mClangErrorHandler clangProcessHandler -> do
        case (mStdin, mClangErrorHandler) of
          (Just stdin, Just clangErrorHandler) -> do
            L.hPut stdin llvm
            hClose stdin
            clangExitCode <- waitForProcess clangProcessHandler
            runInIO $ raiseIfProcessFailed (T.pack clang) clangExitCode clangErrorHandler
          (Nothing, _) ->
            runInIO $ Throw.raiseError' "couldn't obtain stdin"
          (_, Nothing) ->
            runInIO $ Throw.raiseError' "couldn't obtain stderr"

clangLinkOpt :: [Path Abs File] -> Path Abs File -> String -> [String]
clangLinkOpt objectPathList outputPath additionalOptionStr = do
  let pathList = map toFilePath objectPathList
  ["-Wno-override-module", "-O2", "-o", toFilePath outputPath] ++ pathList ++ words additionalOptionStr

link :: [Path Abs File] -> Path Abs File -> App ()
link objectPathList outputPath = do
  clang <- liftIO getClang
  clangOptString <- getClangOptString
  ensureDir $ parent outputPath
  External.run clang $ clangLinkOpt objectPathList outputPath clangOptString

clangBaseOpt :: Path Abs File -> [String]
clangBaseOpt outputPath =
  [ "-xir",
    "-Wno-override-module",
    "-mllvm",
    "-opaque-pointers",
    "-O2",
    "-c",
    "-",
    "-o",
    toFilePath outputPath
  ]

raiseIfProcessFailed :: T.Text -> ExitCode -> Handle -> App ()
raiseIfProcessFailed procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- liftIO $ TIO.hGetContents h
      Throw.raiseError' $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang ->
      return clang
    Nothing ->
      return "clang"

getClangOptString :: App String
getClangOptString =
  readRef' clangOptString

setClangOptString :: String -> App ()
setClangOptString =
  writeRef' clangOptString
