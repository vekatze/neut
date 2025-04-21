module Rule.ForeignCodType
  ( ForeignCodType (..),
    fromForeignCodType,
  )
where

import Data.Binary
import Rule.BaseLowType qualified as BLT
import Rule.LowType qualified as LT
import Rule.LowType.FromBaseLowType qualified as LT
import GHC.Generics

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
