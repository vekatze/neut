module Entity.Prim where

import Data.Binary
import Entity.PrimOp
import qualified Entity.PrimType as PT
import qualified Entity.PrimValue as PV
import qualified GHC.Generics as G

data Prim
  = Op PrimOp
  | Type PT.PrimType
  | Value PV.PrimValue
  deriving (Show, G.Generic)

instance Binary Prim
