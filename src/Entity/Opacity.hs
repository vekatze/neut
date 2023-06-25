module Entity.Opacity where

import Data.Binary
import GHC.Generics

data Opacity
  = Opaque
  | Transparent
  | Lucent
  deriving (Show, Eq, Generic)

instance Binary Opacity

isOpaque :: Opacity -> Bool
isOpaque o =
  case o of
    Opaque ->
      True
    _ ->
      False
