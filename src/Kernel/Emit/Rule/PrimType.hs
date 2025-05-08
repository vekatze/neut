module Kernel.Emit.Rule.PrimType (emitPrimType) where

import Data.ByteString.Builder
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimNumSize.ToInt
import Language.Common.Rule.PrimType qualified as PT

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
