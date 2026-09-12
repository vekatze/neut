module Kernel.Common.ZenConfig (ZenConfig (..)) where

import Data.Text qualified as T
import Kernel.Common.Allocator (Allocator)
import Kernel.Common.ClangOption
import Kernel.Common.Platform qualified as P
import Logger.Hint (Hint)

data ZenConfig = ZenConfig
  { clangOption :: ClangOption,
    allocator :: Allocator,
    platform :: P.PlatformSelector,
    executeCommand :: Maybe (Hint, T.Text)
  }
  deriving (Show, Eq)
