module Command.FormatEns.Move.FormatEns
  ( format,
  )
where

import Command.Common.Move.Format qualified as Format
import Control.Monad.IO.Class (MonadIO (liftIO))
import Library.CommandParser.Rule.Config.FormatEns
import Library.Error.Rule.EIO (EIO)
import Library.Path.Move.EnsureFileExistence (ensureFileExistence')
import Library.Path.Move.Read (readText)
import Library.Path.Move.Write (printText, writeText)
import Path.IO

format :: Config -> EIO ()
format cfg = do
  path <- resolveFile' $ filePathString cfg
  ensureFileExistence' path
  content <- liftIO $ readText path
  content' <- Format.formatEns path content
  if mustUpdateInPlace cfg
    then liftIO $ writeText path content'
    else liftIO $ printText content'
