module Language.Common.PrimType.ToByteSize (primTypeToByteSize) where

import Language.Common.DataSize qualified as DS
import Language.Common.PrimNumSize.ToInt (floatSizeToInt, intSizeToInt)
import Language.Common.PrimType qualified as PT

primTypeToByteSize :: DS.DataSize -> PT.PrimType -> Integer
primTypeToByteSize dataSize primType =
  case primType of
    PT.Int intSize ->
      bitSizeToByteSize $ intSizeToInt intSize
    PT.Float floatSize ->
      bitSizeToByteSize $ floatSizeToInt floatSize
    PT.Rune ->
      4
    PT.Pointer ->
      fromIntegral $ DS.reify dataSize `div` 8

bitSizeToByteSize :: Int -> Integer
bitSizeToByteSize bitSize =
  fromIntegral $ (bitSize + 7) `div` 8
