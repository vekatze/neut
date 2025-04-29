module Move.Scene.Clarify.Handle.AuxEnv
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
import Move.Context.CompDefinition qualified as CompDefinition
import Rule.Comp
import Rule.DefiniteDescription qualified as DD
import Rule.Ident
import Rule.Opacity qualified as O
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { compAuxEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp))
  }

new :: IO Handle
new = do
  compAuxEnvRef <- newIORef Map.empty
  return $ Handle {..}

get :: Handle -> IO CompDefinition.DefMap
get h =
  readIORef (compAuxEnvRef h)

insert :: Handle -> CompDefinition.DefKey -> CompDefinition.DefValue -> IO ()
insert h k v =
  modifyIORef' (compAuxEnvRef h) $ Map.insert k v

checkIfAlreadyRegistered :: Handle -> CompDefinition.DefKey -> IO Bool
checkIfAlreadyRegistered h k = do
  Map.member k <$> get h

toCompStmtList :: CompDefinition.DefMap -> [CompStmt]
toCompStmtList defMap = do
  map fromDefTuple $ Map.toList defMap

clear :: Handle -> IO ()
clear h = do
  writeIORef (compAuxEnvRef h) mempty
