module Entity.PrimType.ToText (toText) where

import Data.Text qualified as T
import Entity.PrimNumSize.ToText
import Entity.PrimType qualified as PT

toText :: PT.PrimType -> T.Text
toText primNum =
  case primNum of
    PT.Int size ->
      intSizeToText size
    PT.Float size ->
      floatSizeToText size
