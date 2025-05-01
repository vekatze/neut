module Rule.Config.Format (Config (..)) where

import Rule.FileType qualified as FT

data Config = Config
  { filePathString :: FilePath,
    mustUpdateInPlace :: Bool,
    shouldMinimizeImports :: Bool,
    inputFileType :: FT.FileType
  }
