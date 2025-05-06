module CommandParser.Rule.Config.Zen (Config (..)) where

import Main.Rule.BuildMode

data Config = Config
  { filePathString :: FilePath,
    buildMode :: BuildMode,
    args :: [String]
  }
