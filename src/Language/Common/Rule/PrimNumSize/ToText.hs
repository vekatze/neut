module Language.Common.Rule.PrimNumSize.ToText
  ( intSizeToText,
    floatSizeToText,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimNumSize.ToInt

intSizeToText :: IntSize -> T.Text
intSizeToText size =
  "int" <> T.pack (show $ intSizeToInt size)

floatSizeToText :: FloatSize -> T.Text
floatSizeToText size =
  "float" <> T.pack (show $ floatSizeToInt size)
