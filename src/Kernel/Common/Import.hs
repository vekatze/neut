module Kernel.Common.Import
  ( ImportItem (..),
    ImportUse (..),
    ImportedEntry (..),
    MustUpdateTag,
  )
where

import Data.Text qualified as T
import Kernel.Common.Source qualified as Source
import Language.Common.BaseName qualified as BN
import Language.Common.LocalLocator qualified as LL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint
import Path

data ImportItem
  = ImportItem Source.Source [ImportUse]
  | StaticFileKey [(T.Text, (Hint, Path Abs File))]

data ImportUse
  = ImportUse MustUpdateTag SGL.StrictGlobalLocator [ImportedEntry]
  deriving (Show)

data ImportedEntry
  = ImportedName Hint LL.LocalLocator (Maybe (Hint, BN.BaseName))
  | NamespaceView Hint BN.BaseName
  deriving (Show)

type MustUpdateTag = Bool
