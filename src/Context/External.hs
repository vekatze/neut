module Context.External
  ( run,
    ensureExecutables,
  )
where

import Context.App
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Handle
import System.Directory
import System.Exit
import System.Process

run :: String -> [String] -> App ()
run procName optionList = do
  let cmd = proc procName optionList
  withRunInIO $ \runInIO ->
    withCreateProcess cmd {std_err = CreatePipe} $ \_ _ mErrorHandler cmdHandler -> do
      case mErrorHandler of
        Just errorHandler -> do
          exitCode <- waitForProcess cmdHandler
          runInIO $ raiseIfProcessFailed (T.pack procName) exitCode errorHandler
        Nothing ->
          runInIO $ Throw.raiseError' "couldn't obtain stderr"

ensureExecutables :: App ()
ensureExecutables =
  mapM_
    ensureExecutable
    [ "clang",
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
      Throw.raiseError' $ "command not found: " <> T.pack name

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
