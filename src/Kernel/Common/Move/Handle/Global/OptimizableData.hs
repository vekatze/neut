module Kernel.Common.Move.Handle.Global.OptimizableData
  ( new,
    insert,
    lookup,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.Rule.Handle.Global.OptimizableData
import Kernel.Common.Rule.OptimizableData
import Language.Common.Rule.DefiniteDescription qualified as DD
import Prelude hiding (lookup)

new :: IO Handle
new = do
  _optDataMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> OptimizableData -> IO ()
insert h dd grp = do
  modifyIORef' (_optDataMapRef h) $ Map.insert dd grp

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookup h dd = do
  optDataMap <- readIORef (_optDataMapRef h)
  return $ Map.lookup dd optDataMap
