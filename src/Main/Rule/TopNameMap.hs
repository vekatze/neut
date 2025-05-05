module Main.Rule.TopNameMap
  ( TopNameMap,
    PreTopNameMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Language.Common.Rule.DefiniteDescription qualified as DD
import Logger.Rule.Hint
import Main.Rule.GlobalName qualified as GN

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

type PreTopNameMap =
  [(DD.DefiniteDescription, (Hint, GN.GlobalName))]
