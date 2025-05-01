module Rule.Config.Get (Config (..)) where

import Data.Text qualified as T
import Rule.ModuleURL

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL
  }
