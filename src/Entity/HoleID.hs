module Entity.HoleID (HoleID (..)) where

import GHC.Generics

newtype HoleID = HoleID {reify :: Int}
  deriving (Show, Eq, Ord, Generic)
