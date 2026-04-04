module Kernel.Clarify.Internal.Handle.AuxEnv
  ( Handle,
    new,
    insert,
    get,
    clear,
    checkIfAlreadyRegistered,
    toCompStmtList,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Language.Comp.Comp
import Prelude hiding (lookup)

newtype Handle = Handle
  { compAuxEnvRef :: IORef (Map.HashMap DD.DefiniteDescription CompStmt)
  }

new :: IO Handle
new = do
  compAuxEnvRef <- newIORef Map.empty
  return $ Handle {..}

get :: Handle -> IO (Map.HashMap DD.DefiniteDescription CompStmt)
get h =
  readIORef (compAuxEnvRef h)

insert :: Handle -> DD.DefiniteDescription -> CompStmt -> IO ()
insert h k v =
  modifyIORef' (compAuxEnvRef h) $ Map.insert k v

checkIfAlreadyRegistered :: Handle -> DD.DefiniteDescription -> IO Bool
checkIfAlreadyRegistered h k = do
  Map.member k <$> get h

toCompStmtList :: Map.HashMap DD.DefiniteDescription CompStmt -> [CompStmt]
toCompStmtList defMap = do
  Map.elems defMap

clear :: Handle -> IO ()
clear h = do
  writeIORef (compAuxEnvRef h) mempty
