module Entity.Config.Release (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Log qualified as Log

data Config = Config
  { getReleaseName :: T.Text,
    logCfg :: Log.Config
  }
