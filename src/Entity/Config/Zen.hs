module Entity.Config.Zen (Config (..)) where

import Data.Text qualified as T
import Entity.BuildMode
import Entity.Config.Remark qualified as Remark

data Config = Config
  { filePathString :: FilePath,
    compileOption :: T.Text,
    linkOption :: T.Text,
    remarkCfg :: Remark.Config,
    buildMode :: BuildMode,
    args :: [String]
  }
