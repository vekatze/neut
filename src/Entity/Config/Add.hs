module Entity.Config.Add (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark
import Entity.ModuleURL

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
    remarkCfg :: Remark.Config
  }
