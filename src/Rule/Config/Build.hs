module Rule.Config.Build (Config (..)) where

import Data.Text qualified as T
import Rule.BuildMode
import Rule.Config.Remark qualified as Remark
import Rule.OutputKind qualified as OK

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
