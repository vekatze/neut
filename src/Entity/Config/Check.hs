module Entity.Config.Check (Config (..)) where

import Entity.Config.Remark qualified as Remark

data Config = Config
  { shouldInsertPadding :: Bool,
    remarkCfg :: Remark.Config
  }
