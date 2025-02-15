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
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Entity.Const (envVarClang)
import Entity.Digest
import Entity.Error
import Entity.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Entity.ProcessRunner.Rule qualified as ProcessRunner
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
  let ProcessRunner.Runner {run00} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = ShellCommand cmd, cwd = Just (toFilePath cwd)}
  value <- liftIO $ run00 spec
  case value of
    Right _ ->
      return $ Right ()
    Left err ->
      Throw.throw $ ProcessRunner.toCompilerError err

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
  let ProcessRunner.Runner {run01} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  output <- liftIO $ run01 spec
  case output of
    Right value ->
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      Throw.throw $ ProcessRunner.toCompilerError err

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
