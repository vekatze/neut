module Rule.Config.Zen (Config (..)) where

import Rule.BuildMode
import Rule.Config.Remark qualified as Remark

data Config = Config
  { filePathString :: FilePath,
    remarkCfg :: Remark.Config,
    buildMode :: BuildMode,
    args :: [String]
  }
