module Entity.Config.Build (Config (..)) where

import Entity.Config.Remark qualified as Remark
import Entity.OutputKind qualified as OK
import Entity.Target

data Config = Config
  { mTarget :: Maybe Target,
    mClangOptString :: Maybe String,
    remarkCfg :: Remark.Config,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    args :: [String]
  }
