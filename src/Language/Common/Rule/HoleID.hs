module Language.Common.Rule.HoleID (HoleID (..)) where

import GHC.Generics

newtype HoleID = HoleID {reify :: Int}
  deriving (Show, Eq, Ord, Generic)
