module Path.Move.Write
  ( writeText,
    writeLazyByteString,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.Encoding
import Path

writeText :: Path Abs File -> T.Text -> IO ()
writeText path content = do
  B.writeFile (toFilePath path) $ encodeUtf8 content

writeLazyByteString :: Path Abs File -> L.ByteString -> IO ()
writeLazyByteString path =
  L.writeFile (toFilePath path)
