module Move.Context.Clang
  ( Handle (..),
    new,
    getClang,
    getClangDigest,
    calculateClangDigest,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Console.Report qualified as Report
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, run)
import Rule.Const (envVarClang)
import Rule.Digest
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import System.Environment (lookupEnv)
import System.Process

data Handle
  = Handle
  { clangDigest :: T.Text,
    debugHandle :: Debug.Handle
  }

new :: Report.Handle -> Debug.Handle -> IO Handle
new reportHandle debugHandle = do
  run reportHandle $ do
    clangDigest <- calculateClangDigest debugHandle
    return $ Handle {..}

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

getClangDigest :: Handle -> T.Text
getClangDigest = do
  clangDigest

calculateClangDigest :: Debug.Handle -> EIO T.Text
calculateClangDigest h = do
  clang <- liftIO getClang
  let ProcessRunner.Runner {run01} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  output <- liftIO $ run01 spec
  case output of
    Right value -> do
      Debug.report h $ "Clang info:\n" <> decodeUtf8 value
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err
