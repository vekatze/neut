module Entity.Config.Check (Config (..)) where

import Entity.Config.Remark qualified as Remark

data Config = Config
  { shouldInsertPadding :: Bool,
    shouldCheckAllDependencies :: Bool,
    remarkCfg :: Remark.Config
  }
