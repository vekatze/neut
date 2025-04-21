module Rule.Config.Get (Config (..)) where

import Data.Text qualified as T
import Rule.Config.Remark qualified as Remark
import Rule.ModuleURL

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
    remarkCfg :: Remark.Config
  }
