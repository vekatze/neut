module Command.FormatEns.Move.FormatEns
  ( format,
  )
where

import Command.Common.Move.Format qualified as Format
import CommandParser.Rule.Config.FormatEns
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Main.Move.Context.Parse (ensureExistence')
import Main.Move.Context.Parse qualified as Parse
import Path.IO
import Path.Move.Read (readText)
import Path.Move.Write (writeText)

format :: Config -> EIO ()
format cfg = do
  path <- resolveFile' $ filePathString cfg
  ensureExistence' path Nothing
  content <- liftIO $ readText path
  content' <- Format.formatEns path content
  if mustUpdateInPlace cfg
    then liftIO $ writeText path content'
    else liftIO $ Parse.printTextFile content'
