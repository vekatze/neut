module Context.Color
  ( setShouldColorizeStdout,
    setShouldColorizeStderr,
    getShouldColorizeStdout,
    getShouldColorizeStderr,
    printStdOut,
    printStdErr,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text.Encoding
import Rule.Log qualified as L
import System.IO

setShouldColorizeStdout :: Bool -> App ()
setShouldColorizeStdout b = do
  b' <- getShouldColorizeStdout
  writeRef' shouldColorizeStdout $ b && b'

setShouldColorizeStderr :: Bool -> App ()
setShouldColorizeStderr b = do
  b' <- getShouldColorizeStderr
  writeRef' shouldColorizeStderr $ b && b'

getShouldColorizeStdout :: App Bool
getShouldColorizeStdout = do
  readRef' shouldColorizeStdout

getShouldColorizeStderr :: App Bool
getShouldColorizeStderr = do
  readRef' shouldColorizeStderr

printStdOut :: L.Log -> App ()
printStdOut l = do
  b <- getShouldColorizeStdout
  let l' = if b then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stdout $ encodeUtf8 l'

printStdErr :: L.Log -> App ()
printStdErr l = do
  b <- getShouldColorizeStderr
  let l' = if b then L.unpackWithSGR l else L.unpackWithoutSGR l
  liftIO $ B.hPutStr stderr $ encodeUtf8 l'
