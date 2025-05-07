module Kernel.Rule.ClangOption
  ( ClangOption (..),
    new,
    empty,
  )
where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)

data ClangOption = ClangOption
  { compileOption :: [T.Text],
    linkOption :: [T.Text]
  }
  deriving (Show, Eq, Generic)

instance Hashable ClangOption

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
