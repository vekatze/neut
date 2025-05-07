module Kernel.Rule.ZenConfig (ZenConfig (..)) where

import Kernel.Rule.ClangOption

newtype ZenConfig = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
