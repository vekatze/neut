module Kernel.Common.RuleHandle.Local.Tag
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.LocationTree qualified as LT
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { _tagMapRef :: IORef LT.LocationTree
  }
