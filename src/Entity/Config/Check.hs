module Entity.Config.Check (Config (..)) where

import Entity.Config.Remark qualified as Remark

data Config = Config
  { mFilePathString :: Maybe FilePath,
    shouldInsertPadding :: Bool,
    remarkCfg :: Remark.Config
  }
