module Entity.ModuleURL where

import Data.Text qualified as T

newtype ModuleURL
  = ModuleURL T.Text
  deriving (Show, Ord, Eq)
