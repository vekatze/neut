module Language.Common.PrimType (PrimType (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.PrimNumSize

data PrimType
  = Int IntSize
  | Float FloatSize
  | Rune
  | Text
  | Blob
  | Pointer
  deriving (Show, Eq, Ord)

deriveBinary [''PrimType]
