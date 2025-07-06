module Kernel.Common.Handle.Global.OptimizableData
  ( Handle (..),
    new,
    insert,
    lookup,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
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

insert :: Handle -> DD.DefiniteDescription -> OptimizableData -> IO ()
insert h dd grp = do
  atomicModifyIORef' (_optDataMapRef h) (\mp -> (Map.insert dd grp mp, ()))

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookup h dd = do
  optDataMap <- readIORef (_optDataMapRef h)
  return $ Map.lookup dd optDataMap
