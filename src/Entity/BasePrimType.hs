module Entity.BasePrimType
  ( BasePrimTypeSize (..),
    BasePrimType (..),
    extractSize,
  )
where

import Data.Binary
import Entity.PrimNumSize
import GHC.Generics

data BasePrimTypeSize a
  = Explicit a
  | Implicit a
  deriving (Show, Eq, Ord, Generic)

instance (Binary a) => Binary (BasePrimTypeSize a)

data BasePrimType
  = Int (BasePrimTypeSize IntSize)
  | Float (BasePrimTypeSize FloatSize)
  deriving (Show, Eq, Ord, Generic)

instance Binary BasePrimType

extractSize :: BasePrimTypeSize a -> a
extractSize size =
  case size of
    Explicit s ->
      s
    Implicit s ->
      s
