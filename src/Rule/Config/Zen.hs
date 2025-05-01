module Rule.Config.Zen (Config (..)) where

import Rule.BuildMode

data Config = Config
  { filePathString :: FilePath,
    buildMode :: BuildMode,
    args :: [String]
  }
