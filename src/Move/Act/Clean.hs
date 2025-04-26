module Move.Act.Clean (clean) where

import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Clean qualified as Clean
import Move.Scene.Initialize qualified as Initialize
import Rule.Config.Clean
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  h <- Clean.new
  toApp $ Clean.clean h
