module CommandParser.Config.Check (Config (..)) where

import Data.Text qualified as T

data Config = Config
  { shouldCheckAllDependencies :: Bool,
    targetName :: Maybe T.Text
  }
