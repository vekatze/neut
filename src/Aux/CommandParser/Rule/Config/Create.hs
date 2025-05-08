module Aux.CommandParser.Rule.Config.Create (Config (..)) where

import Data.Text qualified as T

data Config = Config
  { moduleName :: T.Text,
    targetName :: Maybe T.Text
  }
