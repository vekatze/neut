module Act.Clean (clean) where

import Context.App
import Entity.Config.Clean
import Scene.Clean qualified as Clean
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  Initialize.initializeCompiler (logCfg cfg) True Nothing
  Clean.clean
