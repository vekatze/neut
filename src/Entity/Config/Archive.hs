module Entity.Config.Archive (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark

data Config = Config
  { getArchiveName :: Maybe T.Text,
    remarkCfg :: Remark.Config
  }
