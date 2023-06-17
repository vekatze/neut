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
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.StrictGlobalLocator qualified as SGL
import Path

data AliasInfo
  = Prefix Hint GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  | Use SGL.StrictGlobalLocator [(Hint, LL.LocalLocator)]
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

getRawAlias :: AliasInfo -> Maybe T.Text
getRawAlias aliasInfo =
  case aliasInfo of
    Prefix _ (GLA.GlobalLocatorAlias rawAlias) _ ->
      Just $ BN.reify rawAlias
    _ ->
      Nothing
