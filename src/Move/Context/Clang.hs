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
import Control.Monad.Reader (asks)
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Debug (report)
import Move.Context.EIO (EIO)
import Rule.Const (envVarClang)
import Rule.Digest
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import System.Environment (lookupEnv)
import System.Process

newtype Handle
  = Handle
  { clangRef :: IORef (Maybe T.Text)
  }

new :: App Handle
new = do
  clangRef <- asks App.clangDigest
  return $ Handle {..}

getClang :: IO String
getClang = do
  mClang <- lookupEnv envVarClang
  case mClang of
    Just clang -> do
      return clang
    Nothing -> do
      return "clang"

getClangDigest :: Handle -> EIO T.Text
getClangDigest h = do
  digestOrNone <- liftIO $ readIORef (clangRef h)
  case digestOrNone of
    Just digest -> do
      return digest
    Nothing -> do
      digest <- calculateClangDigest
      liftIO $ writeIORef (clangRef h) (Just digest)
      return digest

calculateClangDigest :: EIO T.Text
calculateClangDigest = do
  clang <- liftIO getClang
  let ProcessRunner.Runner {run01} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  output <- liftIO $ run01 spec
  case output of
    Right value -> do
      report $ "Clang info:\n" <> decodeUtf8 value
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err
