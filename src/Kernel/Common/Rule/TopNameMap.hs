module Kernel.Common.Rule.TopNameMap
  ( TopNameMap,
  )
where

import Logger.Rule.Hint
import Data.HashMap.Strict qualified as Map
import Kernel.Common.Rule.GlobalName qualified as GN
import Language.Common.Rule.DefiniteDescription qualified as DD

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)
