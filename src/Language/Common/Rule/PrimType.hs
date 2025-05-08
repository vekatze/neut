module Language.Common.Rule.PrimType (PrimType (..)) where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.PrimNumSize

data PrimType
  = Int IntSize
  | Float FloatSize
  | Rune
  | Pointer
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimType
