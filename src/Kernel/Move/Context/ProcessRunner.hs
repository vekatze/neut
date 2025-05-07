module Kernel.Move.Context.ProcessRunner
  ( Spec (..),
    CommandError (..),
    ProcessError (..),
    Input (..),
    run00,
    run01,
    run10,
    run11,
    toCompilerError,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Error.Rule.Error qualified as E
import GHC.IO.Handle (Handle, hClose)
import Language.Common.Rule.Error (newError')
import System.Exit
import System.Process qualified as P

data Spec = Spec
  { cmdspec :: P.CmdSpec,
    cwd :: Maybe FilePath
  }

data CommandError = CommandError
  { spec :: Spec,
    exitCode :: Int,
    errStr :: B.ByteString
  }

data ProcessError
  = CommandExecutionError CommandError
  | SetupError T.Text

data Input
  = Strict B.ByteString
  | Lazy L.ByteString

type Output =
  B.ByteString

toCompilerError :: ProcessError -> E.Error
toCompilerError err =
  case err of
    CommandExecutionError (CommandError {spec = Spec {cmdspec}, exitCode, errStr}) -> do
      let name = case cmdspec of P.ShellCommand c -> c; P.RawCommand cmdName _ -> cmdName
      newError' $
        "The child process `"
          <> T.pack name
          <> "` failed with the following message (exitcode = "
          <> T.pack (show exitCode)
          <> "):\n"
          <> indent (decodeUtf8 errStr)
    SetupError e ->
      newError' e

indent :: T.Text -> T.Text
indent t =
  T.intercalate "\n" $ map ("  " <>) $ T.splitOn "\n" t

run00 :: Spec -> IO (Either ProcessError ())
run00 spec = do
  P.withCreateProcess (fromSpec spec) {P.std_err = P.CreatePipe} $
    \_ _ mErrorHandler processHandle -> do
      case mErrorHandler of
        Nothing ->
          return $ Left stderrError
        Just stdErr -> do
          receiveOutput spec processHandle stdErr $ return ()

run01 :: Spec -> IO (Either ProcessError Output)
run01 spec = do
  P.withCreateProcess (fromSpec spec) {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe} $
    \_ mStdOut mErrorHandler processHandle -> do
      case (mStdOut, mErrorHandler) of
        (Nothing, _) ->
          return $ Left stdoutError
        (_, Nothing) ->
          return $ Left stderrError
        (Just stdOut, Just stdErr) -> do
          receiveOutput spec processHandle stdErr $ B.hGetContents stdOut

run10 :: Spec -> Input -> IO (Either ProcessError ())
run10 spec input = do
  P.withCreateProcess (fromSpec spec) {P.std_in = P.CreatePipe, P.std_err = P.CreatePipe} $
    \mStdIn _ mErrorHandler processHandle -> do
      case (mStdIn, mErrorHandler) of
        (Nothing, _) ->
          return $ Left stdinError
        (_, Nothing) ->
          return $ Left stderrError
        (Just stdIn, Just stdErr) -> do
          sendInput stdIn input
          receiveOutput spec processHandle stdErr $ return ()

run11 :: Spec -> Input -> IO (Either ProcessError Output)
run11 spec input = do
  P.withCreateProcess (fromSpec spec) {P.std_in = P.CreatePipe, P.std_out = P.CreatePipe, P.std_err = P.CreatePipe} $
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

sendInput :: Handle -> Input -> IO ()
sendInput h input = do
  case input of
    Strict strictInput ->
      B.hPut h strictInput
    Lazy lazyInput ->
      L.hPut h lazyInput
  hClose h

createError :: Spec -> Int -> Handle -> IO CommandError
createError spec failureCode h = do
  errStr <- B.hGetContents h
  return $
    CommandError
      { spec = spec,
        exitCode = failureCode,
        errStr = errStr
      }

receiveOutput :: Spec -> P.ProcessHandle -> Handle -> IO a -> IO (Either ProcessError a)
receiveOutput spec processHandle hErr resultReader = do
  exitCode <- P.waitForProcess processHandle
  case exitCode of
    ExitSuccess -> do
      Right <$> resultReader
    ExitFailure failureCode -> do
      Left . CommandExecutionError <$> createError spec failureCode hErr

stdinError :: ProcessError
stdinError =
  SetupError "Could not obtain stdin"

stdoutError :: ProcessError
stdoutError =
  SetupError "Could not obtain stdout"

stderrError :: ProcessError
stderrError =
  SetupError "Could not obtain stderr"

fromSpec :: Spec -> P.CreateProcess
fromSpec spec = do
  let Spec {cmdspec, cwd} = spec
  case cmdspec of
    P.ShellCommand c ->
      (P.shell c) {P.cwd = cwd}
    P.RawCommand cmd args ->
      (P.proc cmd args) {P.cwd = cwd}
