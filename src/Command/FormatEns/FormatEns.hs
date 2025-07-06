module Command.FormatEns.FormatEns
  ( format,
  )
where

import Command.Common.Format qualified as Format
import CommandParser.Config.FormatEns
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.EIO (EIO)
import Path.IO
import Path.Read (isStdin, readTextFromPathOrStdin)
import Path.Write (printText, writeText)

format :: Config -> EIO ()
format cfg = do
  path <- resolveFile' (filePathString cfg)
  content <- readTextFromPathOrStdin path
  content' <- Format.formatEns path content
  if mustUpdateInPlace cfg && not (isStdin path)
    then liftIO $ writeText path content'
    else liftIO $ printText content'
