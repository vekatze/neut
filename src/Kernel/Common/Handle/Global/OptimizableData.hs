module Kernel.Common.Handle.Global.OptimizableData
  ( Handle (..),
    new,
    insertMany,
    lookup,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.HashMapRef (insertEntries)
import Kernel.Common.OptimizableData
import Language.Common.DefiniteDescription qualified as DD
import Prelude hiding (lookup)

newtype Handle = Handle
  { _optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData)
  }

new :: IO Handle
new = do
  _optDataMapRef <- newIORef Map.empty
  return $ Handle {..}

insertMany :: Handle -> [(DD.DefiniteDescription, OptimizableData)] -> IO ()
insertMany h =
  insertEntries (_optDataMapRef h)

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookup h dd = do
  optDataMap <- readIORef (_optDataMapRef h)
  return $ Map.lookup dd optDataMap
