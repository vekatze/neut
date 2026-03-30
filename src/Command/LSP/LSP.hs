module Command.LSP.LSP
  ( lsp,
  )
where

import App.App (App)
import Command.LSP.Internal.Server qualified as L
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))

lsp :: App ()
lsp = do
  void $ liftIO L.lsp
