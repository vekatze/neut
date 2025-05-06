module Main.Move.Act.FormatEns
  ( format,
  )
where

import CommandParser.Rule.Config.FormatEns
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Main.Move.Context.Parse (ensureExistence')
import Main.Move.Context.Parse qualified as Parse
import Main.Move.Scene.Format qualified as Format
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
