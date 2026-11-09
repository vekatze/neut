module Kernel.Common.TargetConfig
  ( TargetConfig (..),
    emptyTargetConfig,
  )
where

import Data.Text qualified as T
import Kernel.Common.Allocator (Allocator, defaultAllocator)
import Kernel.Common.ClangOption
import Logger.Hint (Hint)

data TargetConfig = TargetConfig
  { clangOption :: ClangOption,
    allocator :: Allocator,
    executeCommand :: Maybe (Hint, T.Text)
  }
  deriving (Show, Eq)

emptyTargetConfig :: TargetConfig
emptyTargetConfig =
  TargetConfig {clangOption = empty, allocator = defaultAllocator, executeCommand = Nothing}
