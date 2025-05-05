module Main.Move.Context.External
  ( Handle,
    new,
    run,
    runOrFail,
    runOrFail',
    expandText,
    ensureExecutable,
    ExternalError (..),
  )
where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding
import Error.Rule.EIO (EIO)
import Error.Rule.Error
import Language.Common.Move.Raise (raiseError')
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.ProcessRunner qualified as ProcessRunner
import Path
import System.Directory
import System.Process

newtype Handle = Handle
  { loggerHandle :: Logger.Handle
  }

new :: Logger.Handle -> Handle
new loggerHandle = do
  Handle {..}

run :: Handle -> String -> [String] -> EIO ()
run h procName optionList = do
  runOrFail h procName optionList >>= liftEither

runOrFail :: Handle -> String -> [String] -> EIO (Either Error ())
runOrFail h procName optionList = do
  liftIO $ Logger.report (loggerHandle h) $ "Executing: " <> T.pack (show (procName, optionList))
  let spec = ProcessRunner.Spec {cmdspec = RawCommand procName optionList, cwd = Nothing}
  value <- liftIO $ ProcessRunner.run00 spec
  case value of
    Right _ ->
      return $ Right ()
    Left err ->
      throwError $ ProcessRunner.toCompilerError err

data ExternalError = ExternalError
  { cmd :: String,
    exitCode :: Int,
    errStr :: T.Text
  }

runOrFail' :: Handle -> Path Abs Dir -> String -> EIO (Either ExternalError ())
runOrFail' h cwd cmd = do
  liftIO $ Logger.report (loggerHandle h) $ "Executing: " <> T.pack cmd <> "\n(cwd = " <> T.pack (toFilePath cwd) <> ")"
  let spec = ProcessRunner.Spec {cmdspec = ShellCommand cmd, cwd = Just (toFilePath cwd)}
  value <- liftIO $ ProcessRunner.run00 spec
  case value of
    Right _ ->
      return $ Right ()
    Left err ->
      throwError $ ProcessRunner.toCompilerError err

ensureExecutable :: String -> EIO ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      raiseError' $ "Command not found: " <> T.pack name

expandText :: T.Text -> EIO T.Text
expandText t = do
  let spec =
        ProcessRunner.Spec
          { cmdspec = RawCommand "sh" ["-c", unwords [T.unpack "printf", "%s", "\"" ++ T.unpack t ++ "\""]],
            cwd = Nothing
          }
  output <- liftIO $ ProcessRunner.run01 spec
  case output of
    Right value ->
      return $ decodeUtf8 value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err
