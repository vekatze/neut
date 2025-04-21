module Rule.ZenConfig (ZenConfig (..)) where

import Rule.ClangOption

newtype ZenConfig
  = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
