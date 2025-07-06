module Kernel.Common.TopNameMap
  ( TopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Kernel.Common.GlobalName qualified as GN
import Language.Common.DefiniteDescription qualified as DD
import Logger.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)
