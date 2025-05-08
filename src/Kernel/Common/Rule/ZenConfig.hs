module Kernel.Common.Rule.ZenConfig (ZenConfig (..)) where

import Kernel.Common.Rule.ClangOption

newtype ZenConfig = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
