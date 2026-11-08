module Language.Common.ArgNum
  ( ArgNum,
    fromInt,
    zero,
    reify,
    argNumS4,
    add,
  )
where

import Binary.Derive (deriveBinary)
import Prelude hiding (succ)

newtype ArgNum = MakeArgNum {reify :: Int}
  deriving (Eq, Show)

fromInt :: Int -> ArgNum
fromInt i =
  MakeArgNum {reify = i}

zero :: ArgNum
zero =
  MakeArgNum {reify = 0}

-- S4@(switch, target, extra)
argNumS4 :: ArgNum
argNumS4 =
  MakeArgNum 3

add :: ArgNum -> ArgNum -> ArgNum
add (MakeArgNum i1) (MakeArgNum i2) =
  MakeArgNum (i1 + i2)

deriveBinary [''ArgNum]
