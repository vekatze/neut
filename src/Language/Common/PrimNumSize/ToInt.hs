module Language.Common.PrimNumSize.ToInt
  ( intSizeToInt,
    floatSizeToInt,
  )
where

import Language.Common.PrimNumSize

intSizeToInt :: IntSize -> Int
intSizeToInt intSize =
  case intSize of
    IntSize1 ->
      1
    IntSize2 ->
      2
    IntSize4 ->
      4
    IntSize8 ->
      8
    IntSize16 ->
      16
    IntSize32 ->
      32
    IntSize64 ->
      64

floatSizeToInt :: FloatSize -> Int
floatSizeToInt floatSize =
  case floatSize of
    FloatSize16 ->
      16
    FloatSize32 ->
      32
    FloatSize64 ->
      64
