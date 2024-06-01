module Entity.ZenConfig (ZenConfig (..)) where

import Entity.ClangOption

newtype ZenConfig
  = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
