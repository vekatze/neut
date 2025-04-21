module Rule.PrimNumSize.ToInt
  ( intSizeToInt,
    floatSizeToInt,
  )
where

import Rule.PrimNumSize

intSizeToInt :: IntSize -> Int
intSizeToInt intSize =
  case intSize of
    IntSize size ->
      size

floatSizeToInt :: FloatSize -> Int
floatSizeToInt floatSize =
  case floatSize of
    FloatSize16 ->
      16
    FloatSize32 ->
      32
    FloatSize64 ->
      64
