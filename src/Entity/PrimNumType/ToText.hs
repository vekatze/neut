module Entity.PrimNumType.ToText (toText) where

import qualified Data.Text as T
import qualified Entity.PrimNumType as PNT
import Entity.PrimNumSize.ToText

toText :: PNT.PrimNumType -> T.Text
toText primNum =
  case primNum of
    PNT.Int size ->
      intSizeToText size
    PNT.Float size ->
      floatSizeToText size
