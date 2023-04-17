module Entity.Config.Clean (Config (..)) where

import Entity.Config.Remark qualified as Remark
import Prelude hiding (remark)

newtype Config = Config
  { remarkCfg :: Remark.Config
  }
