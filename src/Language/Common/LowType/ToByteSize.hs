module Language.Common.LowType.ToByteSize (lowTypeToByteSize) where

import Language.Common.DataSize qualified as DS
import Language.Common.LowType qualified as LT
import Language.Common.PrimType.ToByteSize (primTypeToByteSize)

lowTypeToByteSize :: DS.DataSize -> LT.LowType -> Integer
lowTypeToByteSize dataSize lowType =
  case lowType of
    LT.PrimNum primType ->
      primTypeToByteSize dataSize primType
    LT.Pointer ->
      fromIntegral $ DS.reify dataSize `div` 8
    LT.Array len elemType ->
      fromIntegral len * lowTypeToByteSize dataSize elemType
    LT.Struct ts ->
      sum $ map (lowTypeToByteSize dataSize) ts
    LT.Function {} ->
      fromIntegral $ DS.reify dataSize `div` 8
    LT.Void ->
      0
    LT.VarArgs ->
      0
