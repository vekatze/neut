module Context.External
  ( run,
    runOrFail,
    runOrFail',
    getClang,
    getClangDigest,
    ensureExecutables,
    expandText,
    raiseIfProcessFailed,
    calculateClangDigest,
    ExternalError (..),
  )
where

import Context.App
import Context.App.Internal
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Entity.Const (envVarClang)
import Entity.Digest
import Entity.Error
import GHC.IO.Handle
import Path
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.Process

run :: String -> [String] -> App ()
run procName optionList = do
  runOrFail procName optionList >>= liftEither

runOrFail :: String -> [String] -> App (Either Error ())
runOrFail procName optionList = do
  let cmd = proc procName optionList
  withRunInIO $ \runInIO ->
    withCreateProcess cmd {std_err = CreatePipe} $ \_ _ mErrorHandler cmdHandler -> do
      case mErrorHandler of
        Nothing ->
          runInIO $ Throw.raiseError' "Could not obtain stderr"
        Just errorHandler -> do
          exitCode <- waitForProcess cmdHandler
          case exitCode of
            ExitSuccess ->
              return $ Right ()
            ExitFailure i -> do
              errStr <- liftIO $ decodeUtf8 <$> B.hGetContents errorHandler
              return $
                Left $
                  newError' $
                    "The child process `"
                      <> T.pack procName
                      <> "` failed with the following message (exitcode = "
                      <> T.pack (show i)
                      <> "):\n"
                      <> errStr

data ExternalError = ExternalError
  { cmd :: String,
    exitCode :: Int,
    errStr :: T.Text
  }

runOrFail' :: Path Abs Dir -> String -> App (Either ExternalError ())
runOrFail' cwd cmd = do
  let sh = shellWithCwd (toFilePath cwd) cmd
  withRunInIO $ \runInIO ->
    withCreateProcess sh {std_err = CreatePipe} $ \_ _ mErrorHandler cmdHandler -> do
      case mErrorHandler of
        Nothing ->
          runInIO $ Throw.raiseError' "Could not obtain stderr"
        Just errorHandler -> do
          exitCode <- waitForProcess cmdHandler
          case exitCode of
            ExitSuccess -> do
              return $ Right ()
            ExitFailure i -> do
              errStr <- liftIO $ decodeUtf8 <$> B.hGetContents errorHandler
              return $
                Left $
                  ExternalError
                    { cmd = cmd,
                      exitCode = i,
                      errStr = errStr
                    }

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

getClangDigest :: App T.Text
getClangDigest = do
  digestOrNone <- readRefMaybe clangDigest
  case digestOrNone of
    Just digest -> do
      return digest
    Nothing -> do
      digest <- calculateClangDigest
      writeRef clangDigest digest
      return digest

calculateClangDigest :: App T.Text
calculateClangDigest = do
  clang <- liftIO getClang
  let clangCmd = proc clang ["-v"]
  withRunInIO $ \runInIO ->
    withCreateProcess clangCmd {std_err = CreatePipe} $
      \_ _ mStdErr clangProcessHandler -> do
        case mStdErr of
          Just stdErr -> do
            value <- B.hGetContents stdErr
            clangExitCode <- waitForProcess clangProcessHandler
            runInIO $ raiseIfProcessFailed (T.pack clang) clangExitCode stdErr
            return $ decodeUtf8 $ hashAndEncode value
          Nothing ->
            runInIO $ Throw.raiseError' "Could not obtain stderr"

ensureExecutables :: App ()
ensureExecutables = do
  clang <- liftIO getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]

ensureExecutable :: String -> App ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      Throw.raiseError' $ "Command not found: " <> T.pack name

shellWithCwd :: FilePath -> String -> CreateProcess
shellWithCwd cwd str =
  CreateProcess
    { cmdspec = ShellCommand str,
      cwd = Just cwd,
      env = Nothing,
      std_in = Inherit,
      std_out = Inherit,
      std_err = Inherit,
      close_fds = False,
      create_group = False,
      delegate_ctlc = False,
      detach_console = False,
      create_new_console = False,
      new_session = False,
      child_group = Nothing,
      child_user = Nothing,
      use_process_jobs = False
    }

expandText :: T.Text -> App T.Text
expandText t = do
  let printf = "printf"
  let printfCmd = proc "sh" ["-c", unwords [T.unpack printf, "%s", "\"" ++ T.unpack t ++ "\""]]
  withRunInIO $ \runInIO ->
    withCreateProcess printfCmd {std_out = CreatePipe, std_err = CreatePipe} $
      \_ mStdOut mClangErrorHandler printfProcessHandler -> do
        case (mStdOut, mClangErrorHandler) of
          (Just stdOut, Just stdErr) -> do
            value <- B.hGetContents stdOut
            printfExitCode <- waitForProcess printfProcessHandler
            runInIO $ raiseIfProcessFailed printf printfExitCode stdErr
            errorMessage <- liftIO $ decodeUtf8 <$> B.hGetContents stdErr
            unless (T.null errorMessage) $ do
              runInIO $
                Throw.raiseError' $
                  "Expanding the text\n"
                    <> indent t
                    <> "\nfailed with the following message:\n"
                    <> indent errorMessage
            return $ decodeUtf8 value
          (Nothing, _) ->
            runInIO $ Throw.raiseError' "Could not obtain stdout"
          (_, Nothing) ->
            runInIO $ Throw.raiseError' "Could not obtain stderr"

raiseIfProcessFailed :: T.Text -> ExitCode -> Handle -> App ()
raiseIfProcessFailed procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- liftIO $ decodeUtf8 <$> B.hGetContents h
      Throw.raiseError' $
        "The child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> indent errStr

indent :: T.Text -> T.Text
indent t =
  T.intercalate "\n" $ map ("  " <>) $ T.splitOn "\n" t
