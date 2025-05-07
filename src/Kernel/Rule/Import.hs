module Kernel.Rule.Import (ImportItem (..)) where

import Data.Text qualified as T
import Kernel.Rule.AliasInfo qualified as AI
import Kernel.Rule.Source qualified as Source
import Logger.Rule.Hint
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | StaticKey [(T.Text, (Hint, Path Abs File))]
