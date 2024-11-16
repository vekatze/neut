module Entity.LowType.FromBaseLowType (fromBaseLowType) where

import Entity.BaseLowType qualified as BLT
import Entity.LowType qualified as LT
import Entity.PrimType.FromWeakPrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType rlt = do
  case rlt of
    BLT.PrimNum wpt ->
      LT.PrimNum $ fromWeakPrimType wpt
    BLT.Pointer ->
      LT.Pointer
    BLT.Void ->
      LT.Void
