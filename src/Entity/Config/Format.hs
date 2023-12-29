module Entity.Config.Format (Config (..)) where

import Entity.Config.Remark qualified as Remark

data Config = Config
  { remarkCfg :: Remark.Config,
    filePathString :: FilePath,
    mustUpdateInPlace :: Bool
  }
