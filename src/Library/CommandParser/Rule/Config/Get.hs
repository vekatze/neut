module Library.CommandParser.Rule.Config.Get (Config (..)) where

import Data.Text qualified as T

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURLText :: T.Text
  }
