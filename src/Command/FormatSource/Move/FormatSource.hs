module Command.FormatSource.Move.FormatSource
  ( Handle,
    new,
    format,
  )
where

import CommandParser.Rule.Config.FormatSource
import Error.Rule.EIO (EIO)
import Path.Move.EnsureFileExistence (ensureFileExistence')
import Path.Move.Read (readText)
import Path.Move.Write (printText, writeText)
import Command.Common.Move.Format qualified as Format
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Path.IO

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

format :: Handle -> Config -> EIO ()
format h cfg = do
  path <- resolveFile' $ filePathString cfg
  ensureFileExistence' path
  content <- liftIO $ readText path
  let formatHandle = Format.new (globalHandle h)
  content' <- Format.formatSource formatHandle (shouldMinimizeImports cfg) path content
  if mustUpdateInPlace cfg
    then liftIO $ writeText path content'
    else liftIO $ printText content'
