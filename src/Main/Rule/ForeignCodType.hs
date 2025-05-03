module Main.Rule.ForeignCodType
  ( ForeignCodType (..),
    fromForeignCodType,
  )
where

import Data.Binary
import GHC.Generics
import Main.Rule.BaseLowType qualified as BLT
import Main.Rule.LowType qualified as LT
import Main.Rule.LowType.FromBaseLowType qualified as LT

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
