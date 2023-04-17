module Entity.Config.Remark (Config (..)) where

import Data.Text qualified as T

data Config = Config
  { shouldColorize :: Bool,
    endOfEntry :: T.Text
  }
