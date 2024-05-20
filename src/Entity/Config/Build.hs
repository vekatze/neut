module Entity.Config.Build (Config (..)) where

import Data.Text qualified as T
import Entity.BuildMode
import Entity.Config.Remark qualified as Remark
import Entity.OutputKind qualified as OK

data Config = Config
  { targetName :: T.Text,
    remarkCfg :: Remark.Config,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    installDir :: Maybe FilePath,
    buildMode :: BuildMode,
    args :: [String]
  }
