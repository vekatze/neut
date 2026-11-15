module Language.Common.Arity
  ( Arity,
    fromInt,
    zero,
    reify,
    arityS4,
    add,
  )
where

import Binary.Derive (deriveBinary)
import Prelude hiding (succ)

newtype Arity = MakeArity {reify :: Int}
  deriving (Eq, Show)

fromInt :: Int -> Arity
fromInt i =
  MakeArity {reify = i}

zero :: Arity
zero =
  MakeArity {reify = 0}

-- S4@(switch, target, extra)
arityS4 :: Arity
arityS4 =
  MakeArity 3

add :: Arity -> Arity -> Arity
add (MakeArity i1) (MakeArity i2) =
  MakeArity (i1 + i2)

deriveBinary [''Arity]
