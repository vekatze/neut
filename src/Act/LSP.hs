module Act.LSP (lsp) where

import Context.App
import Control.Monad
import Entity.Config.LSP
import Scene.LSP qualified as L

lsp :: Config -> App ()
lsp cfg = do
  void $ L.lsp $ remarkCfg cfg
