module Entity.LowType.FromRawLowType (fromRawLowType) where

import Data.Text qualified as T
import Entity.DataSize qualified as DS
import Entity.LowType qualified as LT
import Entity.PrimType.FromWeakPrimType
import Entity.RawLowType qualified as RLT

fromRawLowType :: DS.DataSize -> RLT.RawLowType -> Either T.Text LT.LowType
fromRawLowType dataSize rlt = do
  case rlt of
    RLT.PrimNum wpt ->
      LT.PrimNum <$> fromWeakPrimType dataSize wpt
    RLT.Pointer ->
      return LT.Pointer
    RLT.Void ->
      return LT.Void
