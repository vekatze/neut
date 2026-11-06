module Language.Common.HoleID (HoleID (..)) where


newtype HoleID = HoleID {reify :: Int}
  deriving (Show, Eq, Ord)
