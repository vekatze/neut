module Rule.Config.Create (Config (..)) where

import Data.Text qualified as T
import Rule.Config.Remark qualified as Remark

data Config = Config
  { moduleName :: T.Text,
    targetName :: Maybe T.Text,
    remarkCfg :: Remark.Config
  }
