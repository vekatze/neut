module Rule.TopNameMap
  ( TopNameMap,
    PreTopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Rule.DefiniteDescription qualified as DD
import Rule.GlobalName qualified as GN
import Rule.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

type PreTopNameMap =
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
