module Entity.Config.Add (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Log qualified as Log
import Entity.ModuleURL
import Prelude hiding (log)

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
    logCfg :: Log.Config
  }
