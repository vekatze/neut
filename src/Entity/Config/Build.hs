module Entity.Config.Build (Config (..)) where

import Entity.BuildMode
import Entity.Config.Remark qualified as Remark
import Entity.OutputKind qualified as OK
import Entity.Target

data Config = Config
  { mTarget :: Maybe ConcreteTarget,
    mClangOptString :: Maybe String,
    remarkCfg :: Remark.Config,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    installDir :: Maybe FilePath,
    buildMode :: BuildMode,
    args :: [String]
  }
