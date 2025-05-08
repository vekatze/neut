module Library.CommandParser.Rule.Config.Build (Config (..)) where

import Data.Text qualified as T

data Config = Config
  { targetName :: T.Text,
    outputKindTextList :: [T.Text],
    shouldSkipLink :: Bool,
    shouldExecute :: Bool,
    installDir :: Maybe FilePath,
    buildModeString :: String,
    args :: [String]
  }
