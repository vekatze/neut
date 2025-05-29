module Kernel.Common.Rule.AliasInfo
  ( AliasInfo (..),
    MustUpdateTag,
    SourceAliasMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Logger.Rule.Hint
import Path

data AliasInfo
  = Use MustUpdateTag SGL.StrictGlobalLocator [(Hint, LL.LocalLocator)]
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

type MustUpdateTag = Bool
