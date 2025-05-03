module Main.Rule.Import (ImportItem (..)) where

import Data.Text qualified as T
import Main.Rule.AliasInfo qualified as AI
import Main.Rule.Hint
import Main.Rule.Source qualified as Source
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | StaticKey [(T.Text, (Hint, Path Abs File))]
