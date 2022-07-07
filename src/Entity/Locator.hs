module Entity.Locator where

import Entity.Module

data Locator = Locator
  { sourceLocatorModule :: Module,
    sourceLocatorDirNameList :: [DirName],
    sourceLocatorFileName :: FileName
  }

newtype DirName
  = DirName FilePath

newtype FileName
  = FileName FilePath
