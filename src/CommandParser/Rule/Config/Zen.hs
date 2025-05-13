module CommandParser.Rule.Config.Zen (Config (..)) where

data Config = Config
  { filePathString :: FilePath,
    buildModeString :: String,
    args :: [String]
  }
