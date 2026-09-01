module Kernel.Common.RunProcess
  ( Spec (..),
    Input (..),
    Handle,
    new,
    run,
    runProcess,
    run00,
    run01,
    run10,
  )
where

import App.App (App)
import App.Error (newError')
import Control.Concurrent.Async (wait, withAsync)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Handle qualified as GHC
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import System.Exit
import System.Process (waitForProcess)
import System.Process qualified as P

data Spec = Spec
  { cmdspec :: P.CmdSpec,
    cwd :: Maybe FilePath
  }
  deriving (Show)

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

type ErrorText =
  T.Text

newtype Handle = Handle
  { loggerHandle :: Logger.Handle
  }

new :: Logger.Handle -> Handle
new loggerHandle = do
  Handle {..}

toCompilerError :: ProcessError -> ErrorText
toCompilerError err =
  case err of
    CommandExecutionError (CommandError {spec = Spec {cmdspec}, exitCode, errStr}) -> do
      let name = case cmdspec of P.ShellCommand c -> c; P.RawCommand cmdName _ -> cmdName
      "The child process `"
        <> T.pack name
        <> "` failed with the following message (exitcode = "
        <> T.pack (show exitCode)
        <> "):\n"
        <> indent (decodeUtf8 errStr)
    SetupError e ->
      e

indent :: T.Text -> T.Text
indent t =
  T.intercalate "\n" $ map ("  " <>) $ T.splitOn "\n" t

run :: Handle -> String -> [String] -> App ()
run h procName optionList = do
  let spec = Spec {cmdspec = P.RawCommand procName optionList, cwd = Nothing}
  value <- liftIO $ run00 h spec
  case value of
    Right _ ->
      return ()
    Left err ->
      throwError $ newError' err

runProcess :: Handle -> String -> [String] -> IO ExitCode
runProcess h procName optionList = do
  let spec = Spec {cmdspec = P.RawCommand procName optionList, cwd = Nothing}
  reportCommand h spec
  P.withCreateProcess (fromSpec spec) $
    \_ _ _ processHandle -> do
      waitForProcess processHandle

run00 :: Handle -> Spec -> IO (Either ErrorText ())
run00 h spec = do
  reportCommand h spec
  P.withCreateProcess (fromSpec spec) {P.std_err = P.CreatePipe} $
    \_ _ mErrorHandler processHandle -> do
      case mErrorHandler of
        Nothing ->
          return $ Left stderrError
        Just stdErr -> do
          receiveOutput spec processHandle stdErr $ return ()

run01 :: Handle -> Spec -> IO (Either ErrorText Output)
run01 h spec = do
  reportCommand h spec
  P.withCreateProcess (fromSpec spec) {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe} $
    \_ mStdOut mErrorHandler processHandle -> do
      case (mStdOut, mErrorHandler) of
        (Nothing, _) ->
          return $ Left stdoutError
        (_, Nothing) ->
          return $ Left stderrError
        (Just stdOut, Just stdErr) -> do
          receiveOutput spec processHandle stdErr $ B.hGetContents stdOut

run10 :: Handle -> Spec -> Input -> IO (Either ErrorText ())
run10 h spec input = do
  reportCommand h spec
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

sendInput :: GHC.Handle -> Input -> IO ()
sendInput h input = do
  case input of
    Strict strictInput ->
      B.hPut h strictInput
    Lazy lazyInput ->
      L.hPut h lazyInput
  GHC.hClose h

receiveOutput :: Spec -> P.ProcessHandle -> GHC.Handle -> IO a -> IO (Either ErrorText a)
receiveOutput spec processHandle hErr resultReader = do
  withAsync (B.hGetContents hErr) $ \errReader -> do
    result <- resultReader
    exitCode <- P.waitForProcess processHandle
    errStr <- wait errReader
    case exitCode of
      ExitSuccess ->
        return $ Right result
      ExitFailure failureCode ->
        return $
          Left $
            toCompilerError $
              CommandExecutionError $
                CommandError
                  { spec = spec,
                    exitCode = failureCode,
                    errStr = errStr
                  }

stdinError :: ErrorText
stdinError =
  "Could not obtain stdin"

stdoutError :: ErrorText
stdoutError =
  "Could not obtain stdout"

stderrError :: ErrorText
stderrError =
  "Could not obtain stderr"

fromSpec :: Spec -> P.CreateProcess
fromSpec spec = do
  let Spec {cmdspec, cwd} = spec
  case cmdspec of
    P.ShellCommand c ->
      (P.shell c) {P.cwd = cwd}
    P.RawCommand cmd args ->
      (P.proc cmd args) {P.cwd = cwd}

reportCommand :: Handle -> Spec -> IO ()
reportCommand h spec =
  Logger.report (loggerHandle h) $ "Executing: " <> T.pack (show spec)
