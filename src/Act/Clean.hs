module Act.Clean (clean) where

import Context.App
import Rule.Config.Clean
import Scene.Clean qualified as Clean
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Clean.clean
