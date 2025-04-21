module Move.Context.External
  ( run,
    runOrFail,
    runOrFail',
    expandText,
    ensureExecutable,
    raiseIfProcessFailed,
    ExternalError (..),
  )
where

import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import GHC.IO.Handle
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Throw (liftEither)
import Move.Context.Throw qualified as Throw
import Path
import Rule.Error
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import System.Directory
import System.Exit
import System.Process

run :: String -> [String] -> App ()
run procName optionList = do
  runOrFail procName optionList >>= liftEither

runOrFail :: String -> [String] -> App (Either Error ())
runOrFail procName optionList = do
  h <- Debug.new
  toApp $ Debug.report h $ "Executing: " <> T.pack (show (procName, optionList))
  let ProcessRunner.Runner {run00} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand procName optionList, cwd = Nothing}
  value <- liftIO $ run00 spec
  case value of
    Right _ ->
      return $ Right ()
    Left err ->
      Throw.throw $ ProcessRunner.toCompilerError err

data ExternalError = ExternalError
  { cmd :: String,
    exitCode :: Int,
    errStr :: T.Text
  }

runOrFail' :: Path Abs Dir -> String -> App (Either ExternalError ())
runOrFail' cwd cmd = do
  h <- Debug.new
  toApp $ Debug.report h $ "Executing: " <> T.pack cmd <> "\n(cwd = " <> T.pack (toFilePath cwd) <> ")"
  let ProcessRunner.Runner {run00} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = ShellCommand cmd, cwd = Just (toFilePath cwd)}
  value <- liftIO $ run00 spec
  case value of
    Right _ ->
      return $ Right ()
    Left err ->
      Throw.throw $ ProcessRunner.toCompilerError err

ensureExecutable :: String -> App ()
ensureExecutable name = do
  mPath <- liftIO $ findExecutable name
  case mPath of
    Just _ ->
      return ()
    Nothing ->
      Throw.raiseError' $ "Command not found: " <> T.pack name

expandText :: T.Text -> App T.Text
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
      Throw.throw $ ProcessRunner.toCompilerError err

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
