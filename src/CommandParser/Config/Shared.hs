module CommandParser.Config.Shared (Config (..)) where

import CommandParser.Config.Remark qualified as Remark

data Config = Config
  { remarkConfig :: Remark.Config,
    localArchivesPath :: Maybe FilePath
  }
