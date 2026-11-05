module Language.Common.ForeignCodType
  ( ForeignCodType (..),
    fromForeignCodType,
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.BaseLowType qualified as BLT
import Language.Common.LowType qualified as LT
import Language.Common.LowType.FromBaseLowType qualified as LT

data ForeignCodType a
  = Void
  | Cod a
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

fromForeignCodType :: ForeignCodType BLT.BaseLowType -> LT.LowType
fromForeignCodType cod =
  case cod of
    Cod t ->
      LT.fromBaseLowType t
    Void ->
      LT.Void

deriveBinary [''ForeignCodType]
