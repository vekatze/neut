module Aux.Path.Move.Read
  ( readText,
    readByteString,
  )
where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Path

readText :: Path Abs File -> IO T.Text
readText path = do
  if isStdin path
    then decodeUtf8 <$> B.getContents
    else decodeUtf8 <$> readByteString path

readByteString :: Path Abs File -> IO B.ByteString
readByteString path =
  B.readFile $ toFilePath path

isStdin :: Path Abs File -> Bool
isStdin path =
  toFilePath (filename path) == "-"
