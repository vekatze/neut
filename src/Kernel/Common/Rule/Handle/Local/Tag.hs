module Kernel.Common.Rule.Handle.Local.Tag
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Rule.LocationTree qualified as LT
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { _tagMapRef :: IORef LT.LocationTree
  }
