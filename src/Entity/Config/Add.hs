module Entity.Config.Add (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark
import Entity.ModuleURL
import Prelude hiding (remark)

data Config = Config
  { moduleAliasText :: T.Text,
    moduleURL :: ModuleURL,
    remarkCfg :: Remark.Config
  }
