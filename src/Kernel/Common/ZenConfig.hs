module Kernel.Common.ZenConfig (ZenConfig (..)) where

import Data.Hashable
import GHC.Generics (Generic)
import Kernel.Common.Allocator (Allocator)
import Kernel.Common.ClangOption

data ZenConfig = ZenConfig
  { clangOption :: ClangOption,
    allocator :: Allocator
  }
  deriving (Show, Eq, Generic)

instance Hashable ZenConfig
