module Entity.LibraryURL where

import qualified Data.Text as T

newtype LibraryURL
  = LibraryURL T.Text
  deriving (Show)
