module Rule.Config.Format (Config (..)) where

import Rule.Config.Remark qualified as Remark
import Rule.FileType qualified as FT

data Config = Config
  { remarkCfg :: Remark.Config,
    filePathString :: FilePath,
    mustUpdateInPlace :: Bool,
    shouldMinimizeImports :: Bool,
    inputFileType :: FT.FileType
  }
