module Entity.LowType.FromBaseLowType (fromBaseLowType) where

import Entity.BaseLowType qualified as BLT
import Entity.LowType qualified as LT
import Entity.PrimType.FromBasePrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType lt = do
  case lt of
    BLT.PrimNum bpt ->
      LT.PrimNum $ fromBasePrimType bpt
    BLT.Pointer ->
      LT.Pointer
