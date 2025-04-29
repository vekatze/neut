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
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Rule.Const (envVarClang)
import Rule.Digest
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import System.Environment (lookupEnv)
import System.Process

data Handle
  = Handle
  { clangRef :: IORef (Maybe T.Text),
    debugHandle :: Debug.Handle
  }

new :: Debug.Handle -> IO Handle
new debugHandle = do
  clangRef <- newIORef Nothing
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
      digest <- calculateClangDigest h
      liftIO $ writeIORef (clangRef h) (Just digest)
      return digest

calculateClangDigest :: Handle -> EIO T.Text
calculateClangDigest h = do
  clang <- liftIO getClang
  let ProcessRunner.Runner {run01} = ProcessRunner.ioRunner
  let spec = ProcessRunner.Spec {cmdspec = RawCommand clang ["--version"], cwd = Nothing}
  output <- liftIO $ run01 spec
  case output of
    Right value -> do
      Debug.report (debugHandle h) $ "Clang info:\n" <> decodeUtf8 value
      return $ decodeUtf8 $ hashAndEncode value
    Left err ->
      throwError $ ProcessRunner.toCompilerError err
