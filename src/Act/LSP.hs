module Act.LSP (lsp) where

import Context.App
import Control.Monad
import Entity.Config.LSP
import Scene.Initialize qualified as Initialize
import Scene.LSP qualified as L

lsp :: Config -> App ()
lsp cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  void L.lsp
