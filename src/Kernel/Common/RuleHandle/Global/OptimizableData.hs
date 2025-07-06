module Kernel.Common.RuleHandle.Global.OptimizableData
  ( Handle (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.OptimizableData
import Language.Common.DefiniteDescription qualified as DD
import Prelude hiding (lookup)

newtype Handle = Handle
  { _optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData)
  }
