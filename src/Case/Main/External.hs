module Case.Main.External
  ( run,
    Context,
  )
where

import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Handle
import System.Exit
import System.Process

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m,
    MonadUnliftIO m
  ) =>
  Context m

run :: Context m => String -> [String] -> m ()
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
