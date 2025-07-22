module Command.FormatEns.FormatEns
  ( format,
  )
where

import App.App (App)
import Command.Common.Format qualified as Format
import CommandParser.Config.FormatEns
import Control.Monad.IO.Class (MonadIO (liftIO))
import Path.IO
import Path.Read (isStdin, readTextFromPathOrStdin)
import Path.Write (printText, writeText)

format :: Config -> App ()
format cfg = do
  path <- resolveFile' (filePathString cfg)
  content <- readTextFromPathOrStdin path
  content' <- Format.formatEns path content
  if mustUpdateInPlace cfg && not (isStdin path)
    then liftIO $ writeText path content'
    else liftIO $ printText content'
