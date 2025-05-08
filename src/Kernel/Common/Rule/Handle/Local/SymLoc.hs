module Kernel.Common.Rule.Handle.Local.SymLoc
  ( Handle (..),
  )
where

import Data.IORef
import Kernel.Common.Rule.LocalVarTree qualified as LVT

newtype Handle = Handle
  { _localVarMapRef :: IORef LVT.LocalVarTree
  }
