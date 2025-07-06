module Kernel.Emit.PrimType (emitPrimType) where

import Data.ByteString.Builder
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt
import Language.Common.PrimType qualified as PT

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
      emitPrimType $ PT.Int IntSize32
    PT.Pointer ->
      "ptr"
