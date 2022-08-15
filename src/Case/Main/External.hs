module Case.Main.External
  ( run,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Throw as Throw
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    withCreateProcess cmd {std_err = CreatePipe} $ \_ _ (Just errorHandler) cmdHandler -> do
      exitCode <- waitForProcess cmdHandler
      runInIO $ raiseIfProcessFailed (T.pack procName) exitCode errorHandler

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
