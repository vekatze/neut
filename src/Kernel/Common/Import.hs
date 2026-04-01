module Kernel.Common.Import (ImportItem (..)) where

import Data.Text qualified as T
import Kernel.Common.AliasInfo qualified as AI
import Kernel.Common.Source qualified as Source
import Logger.Hint
import Path

data ImportItem
  = ImportItem Source.Source [AI.AliasInfo]
  | TextFileKey [(T.Text, (Hint, Path Abs File))]
