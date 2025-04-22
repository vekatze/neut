module Move.Context.OptimizableData
  ( insert,
    lookup,
    insertIO,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal
import Rule.DefiniteDescription qualified as DD
import Rule.OptimizableData
import Prelude hiding (lookup)

insert :: DD.DefiniteDescription -> OptimizableData -> App ()
insert dd grp = do
  modifyRef' optDataMap $ Map.insert dd grp

lookup :: DD.DefiniteDescription -> App (Maybe OptimizableData)
lookup dd =
  Map.lookup dd <$> readRef' optDataMap

insertIO ::
  IORef (Map.HashMap DD.DefiniteDescription OptimizableData) ->
  DD.DefiniteDescription ->
  OptimizableData ->
  IO ()
insertIO ref dd grp = do
  modifyIORef' ref $ Map.insert dd grp
