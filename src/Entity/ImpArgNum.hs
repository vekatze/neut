module Entity.ImpArgNum
  ( ImpArgNum,
    fromInt,
    zero,
    reify,
  )
where

import Data.Binary
import GHC.Generics

newtype ImpArgNum = MakeImpArgNum {reify :: Int}
  deriving (Generic)

instance Binary ImpArgNum

fromInt :: Int -> ImpArgNum
fromInt i =
  MakeImpArgNum {reify = i}

zero :: ImpArgNum
zero =
  MakeImpArgNum {reify = 0}
