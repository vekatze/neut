module Main.Move.Context.Parse
  ( readTextFile,
    writeTextFile,
    printTextFile,
    ensureExistence,
    ensureExistence',
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Language.Common.Rule.Error qualified as E
import Language.Common.Rule.Hint
import Main.Move.Context.EIO (EIO)
import Main.Rule.Source
import Path
import Path.IO

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

printTextFile :: T.Text -> IO ()
printTextFile content = do
  B.putStr $ encodeUtf8 content

ensureExistence :: Source -> EIO ()
ensureExistence source = do
  let path = sourceFilePath source
  ensureExistence' path (sourceHint source)

ensureExistence' :: Path Abs File -> Maybe Hint -> EIO ()
ensureExistence' path mHint = do
  fileExists <- doesFileExist path
  if fileExists
    then return ()
    else do
      let message = T.pack $ "No such file exists: " <> toFilePath path
      case mHint of
        Just m ->
          throwError $ E.newError m message
        Nothing ->
          throwError $ E.newError' message
