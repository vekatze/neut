module Path.EnsureFileExistence
  ( ensureFileExistence,
    ensureFileExistence',
    ensureSourceFileExistence,
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
  _ensureFileExistence path (Just m) $ "No such file exists: " <> T.pack (toFilePath path)

ensureFileExistence' :: Path Abs File -> App ()
ensureFileExistence' path = do
  _ensureFileExistence path Nothing $ "No such file exists: " <> T.pack (toFilePath path)

ensureSourceFileExistence :: Path Abs File -> Hint -> T.Text -> App ()
ensureSourceFileExistence path m locator = do
  _ensureFileExistence path (Just m) $ "No such source file exists: " <> locator

_ensureFileExistence :: Path Abs File -> Maybe Hint -> T.Text -> App ()
_ensureFileExistence path mHint message = do
  fileExists <- doesFileExist path
  if fileExists
    then return ()
    else do
      case mHint of
        Just m ->
          raiseError m message
        Nothing ->
          raiseError' message
