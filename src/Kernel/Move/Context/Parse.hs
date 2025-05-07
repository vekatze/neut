module Kernel.Move.Context.Parse
  ( printTextFile,
    ensureExistence,
    ensureExistence',
  )
where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Error.Rule.EIO (EIO)
import Kernel.Rule.Source
import Language.Common.Move.Raise (raiseError, raiseError')
import Logger.Rule.Hint
import Path
import Path.IO

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
          raiseError m message
        Nothing ->
          raiseError' message
