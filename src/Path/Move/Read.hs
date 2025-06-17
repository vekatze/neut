module Path.Move.Read
  ( readTextFromPath,
    readTextFromPathOrStdin,
    isStdin,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Error.Rule.EIO (EIO)
import Path
import Path.Move.EnsureFileExistence (ensureFileExistence')

readTextFromPath :: Path Abs File -> EIO T.Text
readTextFromPath path = do
  ensureFileExistence' path
  decodeUtf8 <$> liftIO (readByteString path)

readTextFromPathOrStdin :: Path Abs File -> EIO T.Text
readTextFromPathOrStdin path = do
  if isStdin path
    then decodeUtf8 <$> liftIO B.getContents
    else readTextFromPath path

readByteString :: Path Abs File -> IO B.ByteString
readByteString path =
  B.readFile $ toFilePath path

isStdin :: Path Abs File -> Bool
isStdin path =
  toFilePath (filename path) == "-"
