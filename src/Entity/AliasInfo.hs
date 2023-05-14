module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.StrictGlobalLocator qualified as SGL
import Path

data AliasInfo
  = Prefix GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  | Use SGL.StrictGlobalLocator
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]
