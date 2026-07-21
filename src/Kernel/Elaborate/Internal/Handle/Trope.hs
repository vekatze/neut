module Kernel.Elaborate.Internal.Handle.Trope
  ( Handle,
    TropeMap,
    new,
    insert,
    get,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
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

insert :: Handle -> DD.DefiniteDescription -> [Stmt.DefineMeta] -> IO ()
insert h name defineMetaList = do
  atomicModifyIORef' (tropeMapRef h) $ \mp ->
    (Map.insert name defineMetaList mp, ())

get :: Handle -> IO TropeMap
get h =
  readIORef (tropeMapRef h)
