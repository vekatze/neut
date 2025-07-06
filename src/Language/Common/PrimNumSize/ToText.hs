module Language.Common.PrimNumSize.ToText
  ( intSizeToText,
    floatSizeToText,
  )
where

import Data.Text qualified as T
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt

intSizeToText :: IntSize -> T.Text
intSizeToText size =
  "int" <> T.pack (show $ intSizeToInt size)

floatSizeToText :: FloatSize -> T.Text
floatSizeToText size =
  "float" <> T.pack (show $ floatSizeToInt size)
