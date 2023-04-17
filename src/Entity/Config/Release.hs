module Entity.Config.Release (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark

data Config = Config
  { getReleaseName :: T.Text,
    remarkCfg :: Remark.Config
  }
