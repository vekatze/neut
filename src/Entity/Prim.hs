module Entity.Prim (Prim (..)) where

import Data.Binary
import Entity.PrimType qualified as PT
import Entity.PrimValue qualified as PV
import GHC.Generics qualified as G

data Prim a
  = Type PT.PrimType
  | Value (PV.PrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (Prim a)
