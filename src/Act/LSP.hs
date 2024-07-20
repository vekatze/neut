module Act.LSP (lsp) where

import Context.App
import Control.Monad
import Scene.LSP qualified as L

lsp :: App ()
lsp = do
  void L.lsp
