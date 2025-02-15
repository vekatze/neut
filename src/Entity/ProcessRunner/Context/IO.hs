module Entity.ProcessRunner.Context.IO (ioRunner) where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Entity.ProcessRunner.Rule qualified as ProcessRunner
import GHC.IO.Handle (Handle, hClose)
import System.Exit
import System.Process

{-# INLINE ioRunner #-}
ioRunner :: ProcessRunner.Runner IO
ioRunner =
  ProcessRunner.Runner
    { run00 = \spec@(ProcessRunner.Spec {}) -> do
        withCreateProcess (fromSpec spec) {std_err = CreatePipe} $
          \_ _ mErrorHandler processHandle -> do
            case mErrorHandler of
              Nothing ->
                return $ Left stderrError
              Just stdErr -> do
                receiveOutput spec processHandle stdErr $ return (),
      run01 = \spec@(ProcessRunner.Spec {}) -> do
        withCreateProcess (fromSpec spec) {std_out = CreatePipe, std_err = CreatePipe} $
          \_ mStdOut mErrorHandler processHandle -> do
            case (mStdOut, mErrorHandler) of
              (Nothing, _) ->
                return $ Left stdoutError
              (_, Nothing) ->
                return $ Left stderrError
              (Just stdOut, Just stdErr) -> do
                receiveOutput spec processHandle stdErr $ B.hGetContents stdOut,
      run10 = \spec@(ProcessRunner.Spec {}) input -> do
        withCreateProcess (fromSpec spec) {std_in = CreatePipe, std_err = CreatePipe} $
          \mStdIn _ mErrorHandler processHandle -> do
            case (mStdIn, mErrorHandler) of
              (Nothing, _) ->
                return $ Left stdinError
              (_, Nothing) ->
                return $ Left stderrError
              (Just stdIn, Just stdErr) -> do
                sendInput stdIn input
                receiveOutput spec processHandle stdErr $ return (),
      run11 = \spec@(ProcessRunner.Spec {}) input -> do
        withCreateProcess (fromSpec spec) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe} $
          \mStdIn mStdOut mErrorHandler processHandle -> do
            case (mStdIn, mStdOut, mErrorHandler) of
              (Nothing, _, _) ->
                return $ Left stdinError
              (_, Nothing, _) ->
                return $ Left stdoutError
              (_, _, Nothing) ->
                return $ Left stderrError
              (Just stdIn, Just stdOut, Just stdErr) -> do
                sendInput stdIn input
                receiveOutput spec processHandle stdErr $ B.hGetContents stdOut
    }

sendInput :: Handle -> ProcessRunner.Input -> IO ()
sendInput h input = do
  case input of
    ProcessRunner.Strict strictInput ->
      B.hPut h strictInput
    ProcessRunner.Lazy lazyInput ->
      L.hPut h lazyInput
  hClose h

createError :: ProcessRunner.Spec -> Int -> Handle -> IO ProcessRunner.CommandError
createError spec failureCode h = do
  errStr <- B.hGetContents h
  return $
    ProcessRunner.CommandError
      { spec = spec,
        exitCode = failureCode,
        errStr = errStr
      }

receiveOutput :: ProcessRunner.Spec -> ProcessHandle -> Handle -> IO a -> IO (Either ProcessRunner.ProcessError a)
receiveOutput spec processHandle hErr resultReader = do
  exitCode <- waitForProcess processHandle
  case exitCode of
    ExitSuccess -> do
      Right <$> resultReader
    ExitFailure failureCode -> do
      Left . ProcessRunner.CommandExecutionError <$> createError spec failureCode hErr

stdinError :: ProcessRunner.ProcessError
stdinError =
  ProcessRunner.SetupError "Could not obtain stdin"

stdoutError :: ProcessRunner.ProcessError
stdoutError =
  ProcessRunner.SetupError "Could not obtain stdout"

stderrError :: ProcessRunner.ProcessError
stderrError =
  ProcessRunner.SetupError "Could not obtain stderr"

fromSpec :: ProcessRunner.Spec -> CreateProcess
fromSpec spec = do
  let ProcessRunner.Spec {cmdspec, cwd} = spec
  case cmdspec of
    ShellCommand c ->
      (shell c) {cwd = cwd}
    RawCommand cmd args ->
      (proc cmd args) {cwd = cwd}
