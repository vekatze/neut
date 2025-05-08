module Library.CommandParser.Rule.Config.FormatEns (Config (..)) where

data Config = Config
  { filePathString :: FilePath,
    mustUpdateInPlace :: Bool
  }
