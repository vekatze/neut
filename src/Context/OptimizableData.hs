module Context.OptimizableData
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.OptimizableData
import Prelude hiding (lookup)

insert :: DD.DefiniteDescription -> OptimizableData -> App ()
insert dd grp = do
  modifyRef' optDataMap $ Map.insert dd grp

lookup :: DD.DefiniteDescription -> App (Maybe OptimizableData)
lookup dd =
  Map.lookup dd <$> readRef' optDataMap
