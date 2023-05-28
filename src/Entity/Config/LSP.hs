module Entity.Config.LSP (Config (..)) where

import Data.Text qualified as T
import Entity.Config.Remark qualified as Remark

newtype Config = Config
  { remarkCfg :: Remark.Config
  }
