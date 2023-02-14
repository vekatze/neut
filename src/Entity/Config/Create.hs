module Entity.Config.Create (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Log qualified as Log

data Config = Config
  { moduleName :: T.Text,
    logCfg :: Log.Config
  }
