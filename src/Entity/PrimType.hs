module Entity.PrimType where

import Data.Binary
import Entity.PrimNumSize
import GHC.Generics qualified as G

data PrimType
  = Int IntSize
  | UInt IntSize
  | Float FloatSize
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimType
