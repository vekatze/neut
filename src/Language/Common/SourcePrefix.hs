module Language.Common.SourcePrefix
  ( SourcePrefix,
    canImport,
    contains,
    internalOwnerOf,
    isPrefixOf,
  )
where

import Data.Binary
import Data.Hashable
import Data.List qualified as List
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL

newtype SourcePrefix = SourcePrefix [BN.BaseName]
  deriving (Generic, Show, Eq, Ord)

instance Binary SourcePrefix

instance Hashable SourcePrefix

contains :: SourcePrefix -> SL.SourceLocator -> Bool
contains (SourcePrefix prefix) sourceLocator = do
  prefix `List.isPrefixOf` SL.toBaseNameList sourceLocator

isPrefixOf :: SourcePrefix -> SourcePrefix -> Bool
isPrefixOf (SourcePrefix smaller) (SourcePrefix larger) =
  smaller `List.isPrefixOf` larger

internalOwner :: SL.SourceLocator -> Maybe SourcePrefix
internalOwner sourceLocator = do
  internalOwnerOf $ SL.toBaseNameList sourceLocator

internalOwnerOf :: [BN.BaseName] -> Maybe SourcePrefix
internalOwnerOf baseNameList =
  snd $ List.foldl' step ([], Nothing) baseNameList
  where
    step (prefix, owner) baseName = do
      let owner' =
            if isInternalBaseName baseName
              then Just $ SourcePrefix prefix
              else owner
      (prefix ++ [baseName], owner')

canImport :: SGL.StrictGlobalLocator -> SGL.StrictGlobalLocator -> Bool
canImport current target =
  case internalOwner (SGL.sourceLocator target) of
    Nothing ->
      True
    Just owner ->
      SGL.moduleID current == SGL.moduleID target
        && owner `contains` SGL.sourceLocator current

isInternalBaseName :: BN.BaseName -> Bool
isInternalBaseName baseName =
  "_" `T.isPrefixOf` BN.reify baseName
