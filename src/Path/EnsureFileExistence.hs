module Path.EnsureFileExistence
  ( ensureFileExistence,
    ensureFileExistence',
  )
where

import App.App (App)
import App.Run (raiseError, raiseError')
import Data.Text qualified as T
import Logger.Hint
import Path
import Path.IO

ensureFileExistence :: Path Abs File -> Hint -> App ()
ensureFileExistence path m = do
  _ensureFileExistence path (Just m)

ensureFileExistence' :: Path Abs File -> App ()
ensureFileExistence' path = do
  _ensureFileExistence path Nothing

_ensureFileExistence :: Path Abs File -> Maybe Hint -> App ()
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
