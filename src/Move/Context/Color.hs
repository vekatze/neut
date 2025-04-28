module Move.Context.Color
  ( Handle,
    new,
    setShouldColorizeStdout,
    setShouldColorizeStderr,
    getShouldColorizeStdout,
    getShouldColorizeStderr,
    printStdOut,
    printStdErr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.IORef
import Data.Text.Encoding
import Rule.Log qualified as L
import System.IO hiding (Handle)

data Handle
  = Handle
  { shouldColorizeStdoutRef :: IORef Bool,
    shouldColorizeStderrRef :: IORef Bool
  }

new :: IO Handle
new = do
  shouldColorizeStdoutRef <- hIsTerminalDevice stdout >>= newIORef
  shouldColorizeStderrRef <- hIsTerminalDevice stderr >>= newIORef
  return $ Handle {..}

setShouldColorizeStdout :: Handle -> Bool -> IO ()
setShouldColorizeStdout h b = do
  orig <- getShouldColorizeStdout h
  writeIORef (shouldColorizeStdoutRef h) $ b && orig

setShouldColorizeStderr :: Handle -> Bool -> IO ()
setShouldColorizeStderr h b = do
  orig <- getShouldColorizeStderr h
  writeIORef (shouldColorizeStderrRef h) $ b && orig

getShouldColorizeStdout :: Handle -> IO Bool
getShouldColorizeStdout h = do
  readIORef (shouldColorizeStdoutRef h)

getShouldColorizeStderr :: Handle -> IO Bool
getShouldColorizeStderr h = do
  readIORef (shouldColorizeStderrRef h)

printStdOut :: Handle -> L.Log -> IO ()
printStdOut h l = do
  b <- getShouldColorizeStdout h
  let l' = if b then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stdout $ encodeUtf8 l'

printStdErr :: Handle -> L.Log -> IO ()
printStdErr h l = do
  b <- getShouldColorizeStderr h
  let l' = if b then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stderr $ encodeUtf8 l'
