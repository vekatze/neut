module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.StrictGlobalLocator qualified as SGL
import Path

data AliasInfo
  = Prefix Hint GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]
