module Kernel.Common.Rule.Handle.Global.Type
  ( Handle (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.WeakTerm.Rule.WeakTerm
import Prelude hiding (lookup)

newtype Handle = Handle
  { _typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
  }
