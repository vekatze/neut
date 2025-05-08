module Kernel.Common.Rule.ModuleURL (ModuleURL (..)) where

import Data.Text qualified as T

newtype ModuleURL
  = ModuleURL T.Text
  deriving (Show, Ord, Eq)
