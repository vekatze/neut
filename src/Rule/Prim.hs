module Rule.Prim (Prim (..)) where

import Data.Binary
import Rule.PrimType qualified as PT
import Rule.PrimValue qualified as PV
import GHC.Generics qualified as G

data Prim a
  = Type PT.PrimType
  | Value (PV.PrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (Prim a)
