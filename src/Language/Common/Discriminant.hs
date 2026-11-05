module Language.Common.Discriminant
  ( Discriminant (..),
    zero,
    increment,
  )
where

import Binary.Derive (deriveBinary)

newtype Discriminant = MakeDiscriminant {reify :: Integer}
  deriving (Ord, Eq, Show)

zero :: Discriminant
zero =
  MakeDiscriminant {reify = 0}

increment :: Discriminant -> Discriminant
increment d =
  MakeDiscriminant {reify = reify d + 1}

deriveBinary [''Discriminant]
