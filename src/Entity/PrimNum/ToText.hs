module Entity.PrimNum.ToText (toText) where

import qualified Data.Text as T
import Entity.PrimNum
import Entity.PrimNumSize.ToText

toText :: PrimNum -> T.Text
toText primNum =
  case primNum of
    PrimNumInt size ->
      intSizeToText size
    PrimNumFloat size ->
      floatSizeToText size
