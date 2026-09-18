module Command.LSP.LSP
  ( lsp,
  )
where

import App.App (App)
import Command.LSP.Internal.Server qualified as L
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.LocalArchive qualified as LocalArchive

lsp :: LocalArchive.LocalArchiveMap -> App ()
lsp localArchiveMap = do
  void $ liftIO $ L.lsp localArchiveMap
