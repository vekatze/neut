module Language.Common.Rule.Discriminant
  ( Discriminant (..),
    zero,
    increment,
  )
where

import Data.Binary
import GHC.Generics

newtype Discriminant = MakeDiscriminant {reify :: Integer}
  deriving (Generic, Ord, Eq, Show)

instance Binary Discriminant

zero :: Discriminant
zero =
  MakeDiscriminant {reify = 0}

increment :: Discriminant -> Discriminant
increment d =
  MakeDiscriminant {reify = reify d + 1}
