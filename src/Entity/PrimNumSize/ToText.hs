module Entity.PrimNumSize.ToText
  ( signedIntSizeToText,
    unsignedIntSizeToText,
    floatSizeToText,
  )
where

import Data.Text qualified as T
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt

signedIntSizeToText :: IntSize -> T.Text
signedIntSizeToText size =
  "i" <> T.pack (show $ intSizeToInt size)

unsignedIntSizeToText :: IntSize -> T.Text
unsignedIntSizeToText size =
  "u" <> T.pack (show $ intSizeToInt size)

floatSizeToText :: FloatSize -> T.Text
floatSizeToText size =
  "f" <> T.pack (show $ floatSizeToInt size)
