module Entity.PrimType.EmitPrimType (emitPrimType) where

import Data.ByteString.Builder
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimType qualified as PT

emitPrimType :: PT.PrimType -> Builder
emitPrimType lowType =
  case lowType of
    PT.Int i ->
      "i" <> intDec (intSizeToInt i)
    PT.UInt i ->
      "i" <> intDec (intSizeToInt i)
    PT.Float FloatSize16 ->
      "half"
    PT.Float FloatSize32 ->
      "float"
    PT.Float FloatSize64 ->
      "double"
