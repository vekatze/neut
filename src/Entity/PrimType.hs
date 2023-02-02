module Entity.PrimType where

import Data.Binary
import Entity.PrimNumSize
import qualified GHC.Generics as G

data PrimType
  = Int IntSize
  | Float FloatSize
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimType
