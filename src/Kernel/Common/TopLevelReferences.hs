module Kernel.Common.TopLevelReferences (TopLevelReferences (..)) where

import Binary.Derive (deriveBinary)
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Language.Common.DefiniteDescription qualified as DD

data TopLevelReferences = TopLevelReferences
  { referenceMap :: Map.Map DD.DefiniteDescription (S.Set DD.DefiniteDescription),
    externalReferenceSet :: S.Set DD.DefiniteDescription
  }

instance Semigroup TopLevelReferences where
  references1 <> references2 =
    TopLevelReferences
      { referenceMap = Map.unionWith S.union (referenceMap references1) (referenceMap references2),
        externalReferenceSet = S.union (externalReferenceSet references1) (externalReferenceSet references2)
      }

instance Monoid TopLevelReferences where
  mempty =
    TopLevelReferences
      { referenceMap = Map.empty,
        externalReferenceSet = S.empty
      }

deriveBinary [''TopLevelReferences]
