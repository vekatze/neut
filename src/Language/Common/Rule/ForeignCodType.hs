module Language.Common.Rule.ForeignCodType
  ( ForeignCodType (..),
    fromForeignCodType,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.LowType.FromBaseLowType qualified as LT

data ForeignCodType a
  = Void
  | Cod a
  deriving (Generic, Show, Eq, Ord, Functor, Traversable, Foldable)

instance (Binary a) => Binary (ForeignCodType a)

fromForeignCodType :: ForeignCodType BLT.BaseLowType -> LT.LowType
fromForeignCodType cod =
  case cod of
    Cod t ->
      LT.fromBaseLowType t
    Void ->
      LT.Void
