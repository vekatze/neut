module Entity.RawLowType.Decode (decode) where

import Data.Text qualified as T
import Entity.Doc qualified as D
import Entity.RawLowType qualified as RLT
import Entity.WeakPrimType qualified as WPT

decode :: RLT.RawLowType -> D.Doc
decode lt =
  case lt of
    RLT.PrimNum primType ->
      case primType of
        WPT.Int sizeOrNone ->
          D.join [D.text "int", D.text $ decodeSize sizeOrNone]
        WPT.Float sizeOrNone ->
          D.join [D.text "float", D.text $ decodeSize sizeOrNone]
    RLT.Pointer ->
      D.text "pointer"
    RLT.Void ->
      D.text "void"

decodeSize :: Maybe Int -> T.Text
decodeSize sizeOrNone =
  case sizeOrNone of
    Just size ->
      T.pack (show size)
    Nothing ->
      ""
