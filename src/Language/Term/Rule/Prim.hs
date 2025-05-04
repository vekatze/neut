module Language.Term.Rule.Prim (Prim (..)) where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.PrimType qualified as PT
import Language.Term.Rule.PrimValue qualified as PV

data Prim a
  = Type PT.PrimType
  | Value (PV.PrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (Prim a)
