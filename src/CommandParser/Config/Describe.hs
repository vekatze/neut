module CommandParser.Config.Describe (Config (..)) where

import Data.Text qualified as T

newtype Config = Config
  { targetName :: T.Text
  }
