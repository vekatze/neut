module Language.Common.Rule.PrimType.ToText (toText) where

import Data.Text qualified as T
import Language.Common.Rule.PrimNumSize.ToText
import Language.Common.Rule.PrimType qualified as PT

toText :: PT.PrimType -> T.Text
toText primNum =
  case primNum of
    PT.Int size ->
      intSizeToText size
    PT.Float size ->
      floatSizeToText size
    PT.Rune ->
      "rune"
    PT.Pointer ->
      "pointer"
