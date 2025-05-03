module Main.Rule.Config.Get (Config (..)) where

import Data.Text qualified as T
import Main.Rule.ModuleURL

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL
  }
