module Kernel.Move.Context.Parse
  ( ensureExistence',
  )
where

import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError, raiseError')
import Logger.Rule.Hint
import Path
import Path.IO

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
