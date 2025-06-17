module Command.FormatEns.Move.FormatEns
  ( format,
  )
where

import Command.Common.Move.Format qualified as Format
import CommandParser.Rule.Config.FormatEns
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Path.IO
import Path.Move.Read (isStdin, readTextFromPathOrStdin)
import Path.Move.Write (printText, writeText)

format :: Config -> EIO ()
format cfg = do
  path <- resolveFile' (filePathString cfg)
  content <- readTextFromPathOrStdin path
  content' <- Format.formatEns path content
  if mustUpdateInPlace cfg && not (isStdin path)
    then liftIO $ writeText path content'
    else liftIO $ printText content'
