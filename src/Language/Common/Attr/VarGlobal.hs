module Language.Common.Attr.VarGlobal (Attr (..), new, constLike) where

import Binary.Derive (deriveBinary)
import Language.Common.Arity
import Language.Common.IsConstLike
import Language.Common.IsDestPassing

data Attr = Attr
  { arity :: Arity,
    isConstLike :: IsConstLike,
    isDestPassing :: IsDestPassing
  }
  deriving (Show)

new :: Arity -> Attr
new arity =
  Attr {arity = arity, isConstLike = False, isDestPassing = False}

constLike :: Attr
constLike =
  Attr {arity = zero, isConstLike = True, isDestPassing = False}

deriveBinary [''Attr]
