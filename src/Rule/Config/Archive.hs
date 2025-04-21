module Rule.Config.Archive (Config (..)) where

import Data.Text qualified as T
import Rule.Config.Remark qualified as Remark

data Config = Config
  { getArchiveName :: Maybe T.Text,
    remarkCfg :: Remark.Config
  }
