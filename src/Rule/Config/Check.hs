module Rule.Config.Check (Config (..)) where

import Rule.Config.Remark qualified as Remark

data Config = Config
  { shouldInsertPadding :: Bool,
    shouldCheckAllDependencies :: Bool,
    remarkCfg :: Remark.Config
  }
