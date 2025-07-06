module Kernel.Common.RuleHandle.Local.SymLoc
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.LocalVarTree qualified as LVT

newtype Handle = Handle
  { _localVarMapRef :: IORef LVT.LocalVarTree
  }
