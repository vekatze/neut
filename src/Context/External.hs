module Context.External
  ( run,
    runOrFail,
    ensureExecutables,
  )
where

import Context.App
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Entity.Error
import System.Directory
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
          runInIO $ Throw.raiseError' "couldn't obtain stderr"
        Just errorHandler -> do
          exitCode <- waitForProcess cmdHandler
          case exitCode of
            ExitSuccess ->
              return $ Right ()
            ExitFailure i -> do
              errStr <- liftIO $ TIO.hGetContents errorHandler
              return $
                Left $
                  newError' $
                    "the child process `"
                      <> T.pack procName
                      <> "` failed with the following message (exitcode = "
                      <> T.pack (show i)
                      <> "):\n"
                      <> errStr

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
