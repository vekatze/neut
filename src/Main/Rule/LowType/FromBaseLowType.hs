module Main.Rule.LowType.FromBaseLowType (fromBaseLowType) where

import Main.Rule.BaseLowType qualified as BLT
import Main.Rule.LowType qualified as LT
import Main.Rule.PrimType.FromBasePrimType

fromBaseLowType :: BLT.BaseLowType -> LT.LowType
fromBaseLowType lt = do
  case lt of
    BLT.PrimNum bpt ->
      LT.PrimNum $ fromBasePrimType bpt
    BLT.Pointer ->
      LT.Pointer
