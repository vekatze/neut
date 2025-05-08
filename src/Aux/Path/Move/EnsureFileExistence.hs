module Aux.Path.Move.EnsureFileExistence
  ( ensureFileExistence,
    ensureFileExistence',
  )
where

import Aux.Error.Move.Run (raiseError, raiseError')
import Aux.Error.Rule.EIO (EIO)
import Aux.Logger.Rule.Hint
import Data.Text qualified as T
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
