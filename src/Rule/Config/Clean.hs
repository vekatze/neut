module Rule.Config.Clean (Config (..)) where

import Rule.Config.Remark qualified as Remark

newtype Config = Config
  { remarkCfg :: Remark.Config
  }
