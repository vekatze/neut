module Entity.Config.Build (Config (..)) where

import Entity.Config.Log qualified as Log
import Entity.OutputKind qualified as OK
import Entity.Target
import Prelude hiding (log)

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    logCfg :: Log.Config,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool
  }
