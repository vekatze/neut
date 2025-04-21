module Rule.LowType.FromBaseLowType (fromBaseLowType) where

import Rule.BaseLowType qualified as BLT
import Rule.LowType qualified as LT
import Rule.PrimType.FromBasePrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType lt = do
  case lt of
    BLT.PrimNum bpt ->
      LT.PrimNum $ fromBasePrimType bpt
    BLT.Pointer ->
      LT.Pointer
