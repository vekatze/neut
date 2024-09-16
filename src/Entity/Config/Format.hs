module Entity.Config.Format (Config (..)) where

import Entity.Config.Remark qualified as Remark
import Entity.FileType qualified as FT

data Config = Config
  { remarkCfg :: Remark.Config,
    filePathString :: FilePath,
    mustUpdateInPlace :: Bool,
    shouldMinimizeImports :: Bool,
    inputFileType :: FT.FileType
  }
