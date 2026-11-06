module Kernel.Elaborate.Internal.Handle.Trope
  ( Handle,
    TropeMap,
    new,
    insertMany,
    get,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.HashMapRef (insertEntries)
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Stmt qualified as Stmt

type TropeMap =
  Map.HashMap DD.DefiniteDescription [Stmt.DefineMeta]

newtype Handle = Handle
  { tropeMapRef :: IORef TropeMap
  }

new :: IO Handle
new = do
  tropeMapRef <- newIORef Map.empty
  return $ Handle {..}

insertMany :: Handle -> [(DD.DefiniteDescription, [Stmt.DefineMeta])] -> IO ()
insertMany h =
  insertEntries (tropeMapRef h)

get :: Handle -> IO TropeMap
get h =
  readIORef (tropeMapRef h)
