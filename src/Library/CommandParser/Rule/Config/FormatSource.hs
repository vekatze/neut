module Library.CommandParser.Rule.Config.FormatSource (Config (..)) where

data Config = Config
  { filePathString :: FilePath,
    mustUpdateInPlace :: Bool,
    shouldMinimizeImports :: Bool
  }
