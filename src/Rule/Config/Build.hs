module Rule.Config.Build (Config (..)) where

import Data.Text qualified as T
import Rule.BuildMode
import Rule.OutputKind qualified as OK

data Config = Config
  { targetName :: T.Text,
    outputKindList :: [OK.OutputKind],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    installDir :: Maybe FilePath,
    buildMode :: BuildMode,
    args :: [String]
  }
