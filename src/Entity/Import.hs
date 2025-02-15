module Entity.Import (ImportItem (..)) where

import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.Hint
import Entity.Source qualified as Source
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | StaticKey [(T.Text, (Hint, Path Abs File))]
