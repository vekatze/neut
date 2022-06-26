module Entity.SourceLocator where

import Entity.Module

data SourceLocator = SourceLocator
  { sourceLocatorModule :: Module,
    sourceLocatorDirNameList :: [DirName],
    sourceLocatorFileName :: FileName
  }

newtype DirName
  = DirName FilePath

newtype FileName
  = FileName FilePath
