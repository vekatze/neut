module Command.FormatEns.Move.FormatEns
  ( format,
  )
where

import Aux.CommandParser.Rule.Config.FormatEns
import Aux.Error.Rule.EIO (EIO)
import Aux.Path.Move.EnsureFileExistence (ensureFileExistence')
import Aux.Path.Move.Read (readText)
import Aux.Path.Move.Write (printText, writeText)
import Command.Common.Move.Format qualified as Format
import Control.Monad.IO.Class (MonadIO (liftIO))
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
