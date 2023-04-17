module Entity.Config.Build (Config (..)) where

import Entity.Config.Remark qualified as Remark
import Entity.OutputKind qualified as OK
import Entity.Target
import Prelude hiding (remark)

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    remarkCfg :: Remark.Config,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool
  }
