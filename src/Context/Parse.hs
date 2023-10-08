module Context.Parse
  ( readSourceFile,
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
    content <- B.readFile $ toFilePath path
    return $ decodeUtf8 content

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
