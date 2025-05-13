module Kernel.Common.Rule.Import (ImportItem (..)) where

import Logger.Rule.Hint
import Data.Text qualified as T
import Kernel.Common.Rule.AliasInfo qualified as AI
import Kernel.Common.Rule.Source qualified as Source
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | StaticKey [(T.Text, (Hint, Path Abs File))]
