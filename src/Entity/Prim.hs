module Entity.Prim where

import Data.Binary
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import GHC.Generics qualified as G

data Prim
  = Type PT.PrimType
  | Value PV.PrimValue
  deriving (Show, G.Generic)

instance Binary Prim
