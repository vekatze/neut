module Language.Common.BasePrimType
  ( BasePrimTypeSize (..),
    BasePrimType (..),
    extractSize,
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.PrimNumSize

data BasePrimTypeSize a
  = Explicit a
  | Implicit a
  deriving (Show, Eq, Ord)

data BasePrimType
  = Int (BasePrimTypeSize IntSize)
  | Float (BasePrimTypeSize FloatSize)
  deriving (Show, Eq, Ord)

extractSize :: BasePrimTypeSize a -> a
extractSize size =
  case size of
    Explicit s ->
      s
    Implicit s ->
      s

deriveBinary [''BasePrimTypeSize, ''BasePrimType]
