module Path.EnsureFileExistence
  ( ensureFileExistence,
    ensureFileExistence',
  )
where

import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError, raiseError')
import Logger.Hint
import Path
import Path.IO

ensureFileExistence :: Path Abs File -> Hint -> EIO ()
ensureFileExistence path m = do
  _ensureFileExistence path (Just m)

ensureFileExistence' :: Path Abs File -> EIO ()
ensureFileExistence' path = do
  _ensureFileExistence path Nothing

_ensureFileExistence :: Path Abs File -> Maybe Hint -> EIO ()
_ensureFileExistence path mHint = do
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
