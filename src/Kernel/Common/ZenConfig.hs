module Kernel.Common.ZenConfig (ZenConfig (..)) where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kernel.Common.Allocator (Allocator)
import Kernel.Common.ClangOption
import Kernel.Common.Platform qualified as P

data ZenConfig = ZenConfig
  { clangOption :: ClangOption,
    allocator :: Allocator,
    platform :: P.PlatformSelector,
    executeCommand :: Maybe [T.Text]
  }
  deriving (Show, Eq, Generic)

instance Hashable ZenConfig
