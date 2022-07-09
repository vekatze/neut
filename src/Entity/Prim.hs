module Entity.Prim where

import Data.Binary
import Entity.PrimNum
import Entity.PrimOp
import qualified GHC.Generics as G

data Prim
  = Op PrimOp
  | Type PrimNum
  deriving (Show, G.Generic, Eq, Ord)

instance Binary Prim
