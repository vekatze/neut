module Entity.TopNameMap
  ( TopNameMap,
    PreTopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

type PreTopNameMap =
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
