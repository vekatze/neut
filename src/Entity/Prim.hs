module Entity.Prim where

import Data.Binary
import qualified Entity.PrimNumType as PNT
import Entity.PrimOp
import qualified GHC.Generics as G

data Prim
  = Op PrimOp
  | Type PNT.PrimNumType
  deriving (Show, G.Generic, Eq, Ord)

instance Binary Prim
