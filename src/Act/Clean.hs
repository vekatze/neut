module Act.Clean
  ( clean,
    Config (..),
    Context,
  )
where

import Entity.Config.Clean
import Scene.Clean qualified as Clean
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

class
  ( Initialize.Context m,
    Clean.Context m
  ) =>
  Context m

clean :: Context m => Config -> m ()
clean cfg = do
  Initialize.initializeCompiler (logCfg cfg) True
  Clean.clean
