module Kernel.Common.Import
  ( ImportItem (..),
    ImportUse (..),
    ImportedEntry (..),
    Liveness,
    everywhere,
    isUnconditional,
    whereProvided,
    whereNotProvided,
    isLiveIn,
    isItemLiveIn,
    MustUpdateTag,
  )
where

import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.Capability qualified as Capability
import Kernel.Common.Source qualified as Source
import Language.Common.BaseName qualified as BN
import Language.Common.LocalLocator qualified as LL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint
import Path

data ImportItem
  = ImportItem Liveness Source.Source [ImportUse]
  | StaticFileKey [(T.Text, (Hint, Path Abs File))]

newtype Liveness
  = Liveness (S.Set (S.Set Capability.Capability))
  deriving (Eq)

everywhere :: Liveness
everywhere =
  Liveness $ S.fromList Capability.everySubset

isUnconditional :: Liveness -> Bool
isUnconditional =
  (== everywhere)

whereProvided :: Capability.Capability -> Liveness -> Liveness
whereProvided capability (Liveness environmentSet) =
  Liveness $ S.filter (S.member capability) environmentSet

whereNotProvided :: Capability.Capability -> Liveness -> Liveness
whereNotProvided capability (Liveness environmentSet) =
  Liveness $ S.filter (not . S.member capability) environmentSet

isLiveIn :: S.Set Capability.Capability -> Liveness -> Bool
isLiveIn environment (Liveness environmentSet) =
  S.member environment environmentSet

isItemLiveIn :: S.Set Capability.Capability -> ImportItem -> Bool
isItemLiveIn environment item =
  case item of
    ImportItem liveness _ _ ->
      isLiveIn environment liveness
    StaticFileKey _ ->
      True

data ImportUse
  = ImportUse MustUpdateTag SGL.StrictGlobalLocator [ImportedEntry]
  deriving (Show)

data ImportedEntry
  = ImportedName Hint LL.LocalLocator (Maybe (Hint, BN.BaseName))
  | NamespaceView Hint BN.BaseName
  deriving (Show)

type MustUpdateTag = Bool
