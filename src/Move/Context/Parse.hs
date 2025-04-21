module Move.Context.Parse
  ( readTextFile,
    writeTextFile,
    printTextFile,
    ensureExistence,
    ensureExistence',
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.Throw qualified as Throw
import Path
import Path.IO
import Rule.Hint
import Rule.Source

readTextFile :: Path Abs File -> IO T.Text
readTextFile path = do
  if isStdin path
    then decodeUtf8 <$> B.getContents
    else fmap decodeUtf8 $ B.readFile $ toFilePath path

isStdin :: Path Abs File -> Bool
isStdin path =
  toFilePath (filename path) == "-"

writeTextFile :: Path Abs File -> T.Text -> IO ()
writeTextFile path content = do
  B.writeFile (toFilePath path) $ encodeUtf8 content

printTextFile :: T.Text -> App ()
printTextFile content = do
  liftIO $ B.putStr $ encodeUtf8 content

ensureExistence :: Source -> App ()
ensureExistence source = do
  let path = sourceFilePath source
  ensureExistence' path (sourceHint source)

ensureExistence' :: Path Abs File -> Maybe Hint -> App ()
ensureExistence' path mHint = do
  fileExists <- doesFileExist path
  unless fileExists $ do
    let message = T.pack $ "No such file exists: " <> toFilePath path
    case mHint of
      Just m ->
        Throw.raiseError m message
      Nothing ->
        Throw.raiseError' message
