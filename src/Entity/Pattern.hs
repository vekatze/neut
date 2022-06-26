module Entity.Pattern where

import qualified Data.Text as T
import Entity.Binder
import Entity.Hint

type PatternF a =
  (Hint, T.Text, [BinderF a])
