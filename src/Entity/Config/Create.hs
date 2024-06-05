module Entity.Config.Create (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark

data Config = Config
  { moduleName :: T.Text,
    targetName :: Maybe T.Text,
    remarkCfg :: Remark.Config
  }
