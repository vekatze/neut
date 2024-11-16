module Entity.LowType.FromRawLowType (fromRawLowType) where

import Entity.LowType qualified as LT
import Entity.PrimType.FromWeakPrimType
import Entity.RawLowType qualified as RLT

fromRawLowType :: RLT.RawLowType -> LT.LowType
fromRawLowType rlt = do
  case rlt of
    RLT.PrimNum wpt ->
      LT.PrimNum $ fromWeakPrimType wpt
    RLT.Pointer ->
      LT.Pointer
    RLT.Void ->
      LT.Void
    RLT.Word arch ->
      LT.getWordType arch
