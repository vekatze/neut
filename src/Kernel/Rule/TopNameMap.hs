module Kernel.Rule.TopNameMap
  ( TopNameMap,
    PreTopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Kernel.Rule.GlobalName qualified as GN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Logger.Rule.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

type PreTopNameMap =
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
