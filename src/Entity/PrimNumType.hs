module Entity.PrimNumType where

import Data.Binary
import Entity.PrimNumSize
import qualified GHC.Generics as G

data PrimNumType
  = Int IntSize
  | Float FloatSize
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimNumType

