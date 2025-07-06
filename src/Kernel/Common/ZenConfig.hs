module Kernel.Common.ZenConfig (ZenConfig (..)) where

import Kernel.Common.ClangOption

newtype ZenConfig = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
