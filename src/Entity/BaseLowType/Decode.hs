module Entity.BaseLowType.Decode (decode) where

import Data.Text qualified as T
import Entity.BaseLowType qualified as BLT
import Entity.BasePrimType qualified as BPT
import Entity.Doc qualified as D
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt

decode :: BLT.BaseLowType -> D.Doc
decode lt =
  case lt of
    BLT.PrimNum primType ->
      case primType of
        BPT.Int size ->
          D.join [D.text "int", D.text $ showWeakIntSize size]
        BPT.Float size ->
          D.join [D.text "float", D.text $ showWeakFloatSize size]
    BLT.Pointer ->
      D.text "pointer"

showWeakIntSize :: BPT.BasePrimTypeSize IntSize -> T.Text
showWeakIntSize size =
  case size of
    BPT.Explicit (IntSize s) ->
      T.pack (show s)
    BPT.Implicit _ ->
      ""

showWeakFloatSize :: BPT.BasePrimTypeSize FloatSize -> T.Text
showWeakFloatSize size =
  case size of
    BPT.Explicit s ->
      T.pack (show $ floatSizeToInt s)
    BPT.Implicit _ ->
      ""
