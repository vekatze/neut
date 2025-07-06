module Language.Common.LowType.FromBaseLowType (fromBaseLowType) where

import Language.Common.BaseLowType qualified as BLT
import Language.Common.LowType qualified as LT
import Language.Common.PrimType.FromBasePrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType lt = do
  case lt of
    BLT.PrimNum bpt ->
      LT.PrimNum $ fromBasePrimType bpt
    BLT.Pointer ->
      LT.Pointer
