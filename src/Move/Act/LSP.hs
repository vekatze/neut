module Move.Act.LSP (lsp) where

import Move.Context.App
import Control.Monad
import Move.Scene.LSP qualified as L

lsp :: App ()
lsp = do
  void L.lsp
