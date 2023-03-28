module Entity.Mutability (Mutability (..)) where

import Data.Binary (Binary)
import GHC.Generics

data Mutability
  = Mutable
  | Immutable
  deriving (Show, Eq, Generic)

instance Binary Mutability
