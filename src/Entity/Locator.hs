module Entity.Locator where

import Entity.Module
import Path

data Locator = Locator
  { sourceLocatorModule :: Module,
    sourceLocatorFilePath :: Path Rel File
  }
