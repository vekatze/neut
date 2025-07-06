module CommandParser.Config.Zen (Config (..)) where

data Config = Config
  { filePathString :: FilePath,
    buildModeString :: String,
    args :: [String]
  }
