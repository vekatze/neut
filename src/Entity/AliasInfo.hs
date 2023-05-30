module Entity.AliasInfo
  ( AliasInfo (..),
    SourceAliasMap,
    getRawAlias,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.StrictGlobalLocator qualified as SGL
import Path

data AliasInfo
  = Prefix GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  | Use SGL.StrictGlobalLocator
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

getRawAlias :: AliasInfo -> Maybe T.Text
getRawAlias aliasInfo =
  case aliasInfo of
    Prefix (GLA.GlobalLocatorAlias rawAlias) _ ->
      Just $ BN.reify rawAlias
    _ ->
      Nothing
