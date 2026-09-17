module Kernel.Common.ClangOption
  ( ClangOption (..),
    new,
    empty,
  )
where

import Data.Text qualified as T

data ClangOption = ClangOption
  { compileOption :: [T.Text],
    linkOption :: [T.Text]
  }
  deriving (Show, Eq)


new :: [T.Text] -> [T.Text] -> [T.Text] -> ClangOption
new buildOption compileOption linkOption =
  ClangOption
    { compileOption = buildOption ++ compileOption,
      linkOption = buildOption ++ linkOption
    }

empty :: ClangOption
empty =
  ClangOption
    { compileOption = [],
      linkOption = []
    }
