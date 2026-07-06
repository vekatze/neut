module CommandParser.Config.Format (Config (..)) where

import Console.FormatMode (FormatMode)

data Config = Config
  { filePathStringList :: [FilePath],
    formatMode :: FormatMode,
    stdinFilePath :: Maybe FilePath,
    shouldMinimizeImports :: Bool
  }
