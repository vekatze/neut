module Case.Main.LLVM
  ( emit,
    link,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.External as External
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Entity.OutputKind as OK
import GHC.IO.Handle
import Path
import Path.IO
import System.Exit
import System.Process

type ClangOption = String

type LLVMCode = L.ByteString

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m,
    Path.Context m,
    External.Context m,
    MonadUnliftIO m
  ) =>
  Context m

emit :: Context m => LLVMCode -> [(OK.OutputKind, Path Abs File)] -> m ()
emit llvmCode kindPathList = do
  case kindPathList of
    [] ->
      return ()
    (kind, path) : rest -> do
      emit' llvmCode kind path
      emit llvmCode rest

emit' :: Context m => LLVMCode -> OK.OutputKind -> Path Abs File -> m ()
emit' llvmCode kind path = do
  clangOptString <- Env.getClangOptString
  case kind of
    OK.LLVM -> do
      Path.writeByteString path llvmCode
    OK.Asm ->
      emitInner ("-S" : words clangOptString) llvmCode path
    OK.Object ->
      emitInner (words clangOptString) llvmCode path

emitInner :: Context m => [ClangOption] -> L.ByteString -> Path Abs File -> m ()
emitInner additionalClangOptions llvm outputPath = do
  let clangCmd = proc "clang" $ clangBaseOpt outputPath ++ additionalClangOptions
  withRunInIO $ \runInIO ->
    withCreateProcess clangCmd {std_in = CreatePipe, std_err = CreatePipe} $
      \mStdin _ mClangErrorHandler clangProcessHandler -> do
        case (mStdin, mClangErrorHandler) of
          (Just stdin, Just clangErrorHandler) -> do
            L.hPut stdin llvm
            hClose stdin
            clangExitCode <- waitForProcess clangProcessHandler
            runInIO $ raiseIfProcessFailed "clang" clangExitCode clangErrorHandler
          (Nothing, _) ->
            runInIO $ Throw.raiseError' "couldn't obtain stdin"
          (_, Nothing) ->
            runInIO $ Throw.raiseError' "couldn't obtain stderr"

clangLinkOpt :: [Path Abs File] -> Path Abs File -> String -> [String]
clangLinkOpt objectPathList outputPath additionalOptionStr = do
  let pathList = map toFilePath objectPathList
  ["-Wno-override-module", "-O2", "-o", toFilePath outputPath] ++ pathList ++ words additionalOptionStr

link :: Context m => [Path Abs File] -> Path Abs File -> m ()
link objectPathList outputPath = do
  clangOptString <- Env.getClangOptString
  ensureDir $ parent outputPath
  External.run "clang" $ clangLinkOpt objectPathList outputPath clangOptString

clangBaseOpt :: Path Abs File -> [String]
clangBaseOpt outputPath =
  [ "-xir",
    "-Wno-override-module",
    "-O2",
    "-c",
    "-",
    "-o",
    toFilePath outputPath
  ]

raiseIfProcessFailed :: Context m => T.Text -> ExitCode -> Handle -> m ()
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
