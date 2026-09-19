module Command.LSP.LSP
  ( lsp,
  )
where

import App.App (App)
import Command.LSP.Internal.Server qualified as L
import Console.Handle qualified as Console
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.LocalArchive qualified as LocalArchive
import Logger.Handle qualified as Logger

lsp :: Console.Handle -> Logger.Handle -> LocalArchive.LocalArchiveMap -> App ()
lsp consoleHandle loggerHandle localArchiveMap = do
  void $ liftIO $ L.lsp consoleHandle loggerHandle localArchiveMap
