module Main.Rule.ZenConfig (ZenConfig (..)) where

import Main.Rule.ClangOption

newtype ZenConfig = ZenConfig {clangOption :: ClangOption}
  deriving (Show)
