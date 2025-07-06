module CommandParser.Config.FormatSource (Config (..)) where

data Config = Config
  { filePathString :: FilePath,
    mustUpdateInPlace :: Bool,
    shouldMinimizeImports :: Bool
  }
