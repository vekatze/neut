module Color.Print
  ( printStdOut,
    printStdErr,
  )
where

import Color.Handle
import Color.Text qualified as CT
import Data.ByteString qualified as B
import Data.Text.Encoding
import System.IO hiding (Handle)

printStdOut :: Handle -> CT.Text -> IO ()
printStdOut h l = do
  let l' = if _shouldColorizeStdout h then CT._unpackWithSGR l else CT._unpackWithoutSGR l
  B.hPutStr stdout $ encodeUtf8 l'

printStdErr :: Handle -> CT.Text -> IO ()
printStdErr h l = do
  let l' = if _shouldColorizeStderr h then CT._unpackWithSGR l else CT._unpackWithoutSGR l
  B.hPutStr stderr $ encodeUtf8 l'
