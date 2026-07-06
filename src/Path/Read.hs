module Path.Read
  ( readTextFromPath,
    readTextFromStdin,
  )
where

import App.App (App)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Path
import Path.EnsureFileExistence (ensureFileExistence')

readTextFromPath :: Path Abs File -> App T.Text
readTextFromPath path = do
  ensureFileExistence' path
  decodeUtf8 <$> liftIO (readByteString path)

readTextFromStdin :: App T.Text
readTextFromStdin = do
  decodeUtf8 <$> liftIO B.getContents

readByteString :: Path Abs File -> IO B.ByteString
readByteString path =
  B.readFile $ toFilePath path
