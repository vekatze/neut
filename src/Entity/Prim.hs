module Entity.Prim where

import Data.Binary
import Entity.PrimOp
import qualified Entity.PrimType as PT
import qualified GHC.Generics as G

data Prim
  = Op PrimOp
  | Type PT.PrimType
  deriving (Show, G.Generic, Eq, Ord)

instance Binary Prim
