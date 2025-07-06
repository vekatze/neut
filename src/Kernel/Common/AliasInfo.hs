module Kernel.Common.AliasInfo
  ( AliasInfo (..),
    MustUpdateTag,
    SourceAliasMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Language.Common.LocalLocator qualified as LL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint
import Path

data AliasInfo
  = Use MustUpdateTag SGL.StrictGlobalLocator [(Hint, LL.LocalLocator)]
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

type MustUpdateTag = Bool
