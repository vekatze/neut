module Entity.RawLowType.Decode (decode) where

import Data.Text qualified as T
import Entity.Doc qualified as D
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.RawLowType qualified as RLT
import Entity.WeakPrimType qualified as WPT

decode :: RLT.RawLowType -> D.Doc
decode lt =
  case lt of
    RLT.PrimNum primType ->
      case primType of
        WPT.Int size ->
          D.join [D.text "int", D.text $ showWeakIntSize size]
        WPT.Float size ->
          D.join [D.text "float", D.text $ showWeakFloatSize size]
    RLT.Pointer ->
      D.text "pointer"
    RLT.Void ->
      D.text "void"
    RLT.Word _ ->
      D.text "word"

showWeakIntSize :: WPT.WeakSize IntSize -> T.Text
showWeakIntSize size =
  case size of
    WPT.Explicit (IntSize s) ->
      T.pack (show s)
    WPT.Implicit _ ->
      ""

showWeakFloatSize :: WPT.WeakSize FloatSize -> T.Text
showWeakFloatSize size =
  case size of
    WPT.Explicit s ->
      T.pack (show $ floatSizeToInt s)
    WPT.Implicit _ ->
      ""
