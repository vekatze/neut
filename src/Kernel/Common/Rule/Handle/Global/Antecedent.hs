module Kernel.Common.Rule.Handle.Global.Antecedent
  ( Handle (..),
    RevMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Set qualified as S
import Kernel.Common.Rule.Module qualified as M
import Language.Common.Rule.ModuleID qualified as MID
import Prelude hiding (lookup, read)

data Handle = Handle
  { _antecedentMapRef :: IORef (Map.HashMap MID.ModuleID M.Module),
    _reverseAntecedentMapRef :: IORef (Map.HashMap MID.ModuleID (S.Set MID.ModuleID))
  }

type RevMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)
