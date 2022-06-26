module Entity.Checksum where

import qualified Data.Text as T

newtype Checksum
  = Checksum T.Text
  deriving (Show, Ord, Eq)
