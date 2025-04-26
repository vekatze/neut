module Move.Context.External
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

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError', toApp)
import Move.Context.Throw qualified as Throw
import Path
import Rule.Error
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import System.Directory
import System.Process

newtype Handle
  = Handle
  { debugHandle :: Debug.Handle
  }

new :: App Handle
new = do
  debugHandle <- Debug.new
  return $ Handle {..}

run :: String -> [String] -> App ()
run procName optionList = do
  h <- new
  toApp (runOrFail h procName optionList) >>= Throw.liftEither

runOrFail :: Handle -> String -> [String] -> EIO (Either Error ())
runOrFail h procName optionList = do
  Debug.report (debugHandle h) $ "Executing: " <> T.pack (show (procName, optionList))
  let ProcessRunner.Runner {run00} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand procName optionList, cwd = Nothing}
  value <- liftIO $ run00 spec
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
  Debug.report (debugHandle h) $ "Executing: " <> T.pack cmd <> "\n(cwd = " <> T.pack (toFilePath cwd) <> ")"
  let ProcessRunner.Runner {run00} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = ShellCommand cmd, cwd = Just (toFilePath cwd)}
  value <- liftIO $ run00 spec
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
  let ProcessRunner.Runner {run01} = ProcessRunner.ioRunner
  let spec =
        ProcessRunner.Spec
          { cmdspec = RawCommand "sh" ["-c", unwords [T.unpack "printf", "%s", "\"" ++ T.unpack t ++ "\""]],
            cwd = Nothing
          }
  output <- liftIO $ run01 spec
  case output of
    Right value ->
      return $ decodeUtf8 value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err
