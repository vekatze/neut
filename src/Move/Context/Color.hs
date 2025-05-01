module Move.Context.Color
  ( Handle,
    new,
    getShouldColorizeStdout,
    getShouldColorizeStderr,
    printStdOut,
    printStdErr,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text.Encoding
import Rule.Log qualified as L
import System.IO hiding (Handle)

data Handle
  = Handle
  { shouldColorizeStdout :: Bool,
    shouldColorizeStderr :: Bool
  }

new :: Bool -> IO Handle
new shouldColorize = do
  stdoutIsTerminal <- hIsTerminalDevice stdout
  stderrIsTerminal <- hIsTerminalDevice stderr
  let shouldColorizeStdout = shouldColorize && stdoutIsTerminal
  let shouldColorizeStderr = shouldColorize && stderrIsTerminal
  return $ Handle {..}

getShouldColorizeStdout :: Handle -> Bool
getShouldColorizeStdout h = do
  shouldColorizeStdout h

getShouldColorizeStderr :: Handle -> Bool
getShouldColorizeStderr h = do
  shouldColorizeStderr h

printStdOut :: Handle -> L.Log -> IO ()
printStdOut h l = do
  let l' = if getShouldColorizeStdout h then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stdout $ encodeUtf8 l'

printStdErr :: Handle -> L.Log -> IO ()
printStdErr h l = do
  let l' = if getShouldColorizeStderr h then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stderr $ encodeUtf8 l'
