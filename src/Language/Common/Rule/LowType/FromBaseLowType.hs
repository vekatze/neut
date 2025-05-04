module Language.Common.Rule.LowType.FromBaseLowType (fromBaseLowType) where

import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.PrimType.FromBasePrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType lt = do
  case lt of
    BLT.PrimNum bpt ->
      LT.PrimNum $ fromBasePrimType bpt
    BLT.Pointer ->
      LT.Pointer
