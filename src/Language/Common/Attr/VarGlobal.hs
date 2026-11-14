module Language.Common.Attr.VarGlobal (Attr (..), new) where

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

deriveBinary [''Attr]
