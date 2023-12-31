module Context.Parse
  ( readSourceFile,
    writeSourceFile,
    printSourceFile,
    ensureExistence,
  )
where

import Context.App
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Entity.Source
import Path
import Path.IO

readSourceFile :: Path Abs File -> App T.Text
readSourceFile path = do
  liftIO $ do
    if isStdin path
      then decodeUtf8 <$> B.getContents
      else fmap decodeUtf8 $ B.readFile $ toFilePath path

isStdin :: Path Abs File -> Bool
isStdin path =
  toFilePath (filename path) == "-"

writeSourceFile :: Path Abs File -> T.Text -> App ()
writeSourceFile path content = do
  liftIO $ B.writeFile (toFilePath path) $ encodeUtf8 content

printSourceFile :: T.Text -> App ()
printSourceFile content = do
  liftIO $ B.putStr $ encodeUtf8 content

ensureExistence :: Source -> App ()
ensureExistence source = do
  let path = sourceFilePath source
  fileExists <- doesFileExist path
  unless fileExists $ do
    case sourceHint source of
      Just m ->
        Throw.raiseError m $ T.pack $ "no such file exists: " <> toFilePath path
      Nothing ->
        Throw.raiseError' $ T.pack $ "no such file exists: " <> toFilePath path
