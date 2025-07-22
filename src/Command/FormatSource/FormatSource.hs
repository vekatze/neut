module Command.FormatSource.FormatSource
  ( Handle,
    new,
    format,
  )
where

import App.App (App)
import Command.Common.Format qualified as Format
import CommandParser.Config.FormatSource
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Path.IO
import Path.Read (isStdin, readTextFromPathOrStdin)
import Path.Write (printText, writeText)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

format :: Handle -> Config -> App ()
format h cfg = do
  path <- resolveFile' (filePathString cfg)
  content <- readTextFromPathOrStdin path
  let formatHandle = Format.new (globalHandle h)
  content' <- Format.formatSource formatHandle (shouldMinimizeImports cfg) path content
  if mustUpdateInPlace cfg && not (isStdin path)
    then liftIO $ writeText path content'
    else liftIO $ printText content'
