module Entity.PrimType (PrimType (..)) where

import Data.Binary
import Entity.PrimNumSize
import GHC.Generics qualified as G

data PrimType
  = Int IntSize
  | Float FloatSize
  | Rune
  | Pointer
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimType
