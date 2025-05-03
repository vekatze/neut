module Main.Rule.PrimType.EmitPrimType (emitPrimType) where

import Data.ByteString.Builder
import Main.Rule.PrimNumSize
import Main.Rule.PrimNumSize.ToInt
import Main.Rule.PrimType qualified as PT

emitPrimType :: PT.PrimType -> Builder
emitPrimType lowType =
  case lowType of
    PT.Int i ->
      "i" <> intDec (intSizeToInt i)
    PT.Float FloatSize16 ->
      "half"
    PT.Float FloatSize32 ->
      "float"
    PT.Float FloatSize64 ->
      "double"
    PT.Rune ->
      emitPrimType $ PT.Int intSize32
    PT.Pointer ->
      "ptr"
