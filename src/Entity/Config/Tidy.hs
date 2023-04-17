module Entity.Config.Tidy (Config (..)) where

import Entity.Config.Remark qualified as Remark

newtype Config = Config
  { remarkCfg :: Remark.Config
  }
