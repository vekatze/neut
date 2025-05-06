module Main.Move.Act.FormatSource
  ( Handle,
    new,
    format,
  )
where

import CommandParser.Rule.Config.FormatSource
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Main.Move.Context.Parse (ensureExistence')
import Main.Move.Context.Parse qualified as Parse
import Main.Move.Scene.Format qualified as Format
import Main.Move.Scene.Init.Base qualified as Base
import Path.IO
import Path.Move.Read (readText)
import Path.Move.Write (writeText)

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  Handle {..}

format :: Handle -> Config -> EIO ()
format h cfg = do
  path <- resolveFile' $ filePathString cfg
  ensureExistence' path Nothing
  content <- liftIO $ readText path
  let formatHandle = Format.new (baseHandle h)
  content' <- Format.formatSource formatHandle (shouldMinimizeImports cfg) path content
  if mustUpdateInPlace cfg
    then liftIO $ writeText path content'
    else liftIO $ Parse.printTextFile content'
