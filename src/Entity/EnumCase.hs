module Entity.EnumCase (EnumCase (..)) where

import GHC.Generics

newtype EnumCase
  = Int Integer
  deriving (Show, Eq, Ord, Generic)
