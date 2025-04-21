module Rule.Import (ImportItem (..)) where

import Data.Text qualified as T
import Rule.AliasInfo qualified as AI
import Rule.Hint
import Rule.Source qualified as Source
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | StaticKey [(T.Text, (Hint, Path Abs File))]
