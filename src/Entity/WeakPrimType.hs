module Entity.WeakPrimType where

import Data.Binary
import Entity.PrimNumSize
import GHC.Generics

data WeakSize a
  = Explicit a
  | Implicit a
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (WeakSize a)

data WeakPrimType
  = Int (WeakSize IntSize)
  | Float (WeakSize FloatSize)
  deriving (Show, Eq, Ord, Generic)

instance Binary WeakPrimType

extractSize :: WeakSize a -> a
extractSize size =
  case size of
    Explicit s ->
      s
    Implicit s ->
      s
