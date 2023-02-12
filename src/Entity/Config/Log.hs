module Entity.Config.Log (Config (..)) where

import Data.Text qualified as T
import Prelude hiding (log)

data Config = Config
  { shouldColorize :: Bool,
    endOfEntry :: T.Text
  }
