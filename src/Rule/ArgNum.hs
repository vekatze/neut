module Rule.ArgNum
  ( ArgNum,
    fromInt,
    zero,
    reify,
    argNumS4,
    add,
  )
where

import Data.Binary
import GHC.Generics

newtype ArgNum = MakeArgNum {reify :: Int}
  deriving (Generic, Eq, Show)

instance Binary ArgNum

fromInt :: Int -> ArgNum
fromInt i =
  MakeArgNum {reify = i}

zero :: ArgNum
zero =
  MakeArgNum {reify = 0}

-- S4@(switch, target)
argNumS4 :: ArgNum
argNumS4 =
  MakeArgNum 2

add :: ArgNum -> ArgNum -> ArgNum
add (MakeArgNum i1) (MakeArgNum i2) =
  MakeArgNum (i1 + i2)
