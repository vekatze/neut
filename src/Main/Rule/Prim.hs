module Main.Rule.Prim (Prim (..)) where

import Data.Binary
import GHC.Generics qualified as G
import Main.Rule.PrimType qualified as PT
import Main.Rule.PrimValue qualified as PV

data Prim a
  = Type PT.PrimType
  | Value (PV.PrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (Prim a)
