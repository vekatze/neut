module Move.Act.Clean (clean) where

import Move.Context.App
import Rule.Config.Clean
import Move.Scene.Clean qualified as Clean
import Move.Scene.Initialize qualified as Initialize
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Clean.clean
