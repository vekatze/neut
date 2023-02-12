module Entity.Config.Run where

import Entity.Config.Log qualified as Log
import Entity.Target
import Prelude hiding (log)

data Config = Config
  { target :: Target,
    mClangOptString :: Maybe String,
    shouldCancelAlloc :: Bool,
    logCfg :: Log.Config
  }
