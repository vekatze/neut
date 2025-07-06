module Kernel.Common.RuleHandle.Global.Type
  ( Handle (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Language.WeakTerm.WeakTerm
import Prelude hiding (lookup)

newtype Handle = Handle
  { _typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
  }
