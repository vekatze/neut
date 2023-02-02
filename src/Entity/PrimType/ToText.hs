module Entity.PrimType.ToText (toText) where

import qualified Data.Text as T
import Entity.PrimNumSize.ToText
import qualified Entity.PrimType as PT

toText :: PT.PrimType -> T.Text
toText primNum =
  case primNum of
    PT.Int size ->
      intSizeToText size
    PT.Float size ->
      floatSizeToText size
