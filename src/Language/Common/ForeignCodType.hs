module Language.Common.ForeignCodType
  ( ForeignCodType (..),
    fromForeignCodType,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.BaseLowType qualified as BLT
import Language.Common.LowType qualified as LT
import Language.Common.LowType.FromBaseLowType qualified as LT

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
