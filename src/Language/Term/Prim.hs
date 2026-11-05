module Language.Term.Prim (Prim (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.PrimType qualified as PT
import Language.Term.PrimValue qualified as PV

data Prim a
  = Type PT.PrimType
  | Value (PV.PrimValue a)
  deriving (Show)

deriveBinary [''Prim]
