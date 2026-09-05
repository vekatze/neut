module Language.Common.SlotSize
  ( slotBitSize,
    slotByteSize,
    slotIntSize,
    slotFloatSize,
    slotPrimType,
  )
where

import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT

slotBitSize :: Int
slotBitSize =
  64

slotByteSize :: Int
slotByteSize =
  slotBitSize `div` 8

slotIntSize :: IntSize
slotIntSize =
  IntSize64

slotFloatSize :: FloatSize
slotFloatSize =
  FloatSize64

slotPrimType :: PT.PrimType
slotPrimType =
  PT.Int slotIntSize
