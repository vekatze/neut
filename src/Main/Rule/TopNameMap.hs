module Main.Rule.TopNameMap
  ( TopNameMap,
    PreTopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.GlobalName qualified as GN
import Main.Rule.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

type PreTopNameMap =
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
