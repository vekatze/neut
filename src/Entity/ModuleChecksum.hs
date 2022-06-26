module Entity.ModuleChecksum where

import qualified Data.Text as T

newtype ModuleChecksum
  = ModuleChecksum T.Text
  deriving (Show, Ord, Eq)
