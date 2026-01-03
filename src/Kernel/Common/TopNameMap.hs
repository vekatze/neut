module Kernel.Common.TopNameMap
  ( TopNameInfo,
    TopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Kernel.Common.GlobalName qualified as GN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.NominalTag qualified as NT
import Logger.Hint

type TopNameInfo =
  (Hint, Maybe NT.NominalTag, GN.GlobalName)

type TopNameMap =
  Map.HashMap DD.DefiniteDescription TopNameInfo
