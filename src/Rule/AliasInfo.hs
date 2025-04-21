module Rule.AliasInfo
  ( AliasInfo (..),
    MustUpdateTag,
    SourceAliasMap,
    getRawAlias,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Rule.BaseName qualified as BN
import Rule.GlobalLocatorAlias qualified as GLA
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.StrictGlobalLocator qualified as SGL
import Path

data AliasInfo
  = Prefix Hint GLA.GlobalLocatorAlias SGL.StrictGlobalLocator
  | Use MustUpdateTag SGL.StrictGlobalLocator [(Hint, LL.LocalLocator)]
  deriving (Show)

type SourceAliasMap = Map.HashMap (Path Abs File) [AliasInfo]

type MustUpdateTag = Bool

getRawAlias :: AliasInfo -> Maybe T.Text
getRawAlias aliasInfo =
  case aliasInfo of
    Prefix _ (GLA.GlobalLocatorAlias rawAlias) _ ->
      Just $ BN.reify rawAlias
    _ ->
      Nothing
