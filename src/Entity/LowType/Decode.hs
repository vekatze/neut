module Entity.LowType.Decode (decode) where

import Entity.Doc qualified as D
import Entity.LowType qualified as LT
import Entity.PrimType qualified as PT

decode :: LT.LowType -> D.Doc
decode lt =
  case lt of
    LT.PrimNum primType ->
      case primType of
        PT.Int _ ->
          D.text "int"
        PT.Float _ ->
          D.text "float"
    LT.Pointer ->
      D.text "pointer"
    LT.Void ->
      D.text "void"
    _ ->
      D.text "<other>"
