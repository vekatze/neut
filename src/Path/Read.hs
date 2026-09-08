module Path.Read
  ( readTextFromPath,
    readTextFromStdin,
  )
where

import App.App (App)
import App.Run (raiseError')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Path
import Path.EnsureFileExistence (ensureFileExistence')

readTextFromPath :: Path Abs File -> App T.Text
readTextFromPath path = do
  ensureFileExistence' path
  content <- liftIO $ readByteString path
  decode ("The file `" <> T.pack (toFilePath path) <> "`") content

readTextFromStdin :: App T.Text
readTextFromStdin = do
  content <- liftIO B.getContents
  decode "The input" content

decode :: T.Text -> B.ByteString -> App T.Text
decode description content =
  case decodeUtf8' content of
    Right text ->
      return text
    Left _ ->
      raiseError' $ description <> " is not valid UTF-8"

readByteString :: Path Abs File -> IO B.ByteString
readByteString path =
  B.readFile $ toFilePath path
