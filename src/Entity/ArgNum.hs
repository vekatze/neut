module Entity.ArgNum
  ( ArgNum,
    fromInt,
    zero,
    reify,
  )
where

import Data.Binary
import GHC.Generics

newtype ArgNum = MakeArgNum {reify :: Int}
  deriving (Generic, Eq)

instance Binary ArgNum

fromInt :: Int -> ArgNum
fromInt i =
  MakeArgNum {reify = i}

zero :: ArgNum
zero =
  MakeArgNum {reify = 0}
