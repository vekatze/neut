module Move.Context.OptimizableData
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Rule.DefiniteDescription qualified as DD
import Rule.OptimizableData
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData)
  }

new :: IO Handle
new = do
  optDataMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> OptimizableData -> IO ()
insert h dd grp = do
  modifyIORef' (optDataMapRef h) $ Map.insert dd grp

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookup h dd = do
  optDataMap <- readIORef (optDataMapRef h)
  return $ Map.lookup dd optDataMap
