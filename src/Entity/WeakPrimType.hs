module Entity.WeakPrimType where

import Entity.PrimNumSize

data WeakSize a
  = Explicit a
  | Implicit a
  deriving (Show, Eq, Ord)

data WeakPrimType
  = Int (WeakSize IntSize)
  | Float (WeakSize FloatSize)
  deriving (Show, Eq, Ord)
