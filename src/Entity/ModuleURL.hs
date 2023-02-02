module Entity.ModuleURL where

import qualified Data.Text as T

newtype ModuleURL
  = ModuleURL T.Text
  deriving (Show)
