module Entity.Config.Clean (Config (..)) where

import Entity.Config.Log qualified as Log
import Prelude hiding (log)

newtype Config = Config
  { logCfg :: Log.Config
  }
