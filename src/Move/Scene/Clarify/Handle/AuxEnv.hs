module Move.Scene.Clarify.Handle.AuxEnv
  ( Handle,
    new,
    insert,
    get,
    checkIfAlreadyRegistered,
    toCompStmtList,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
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

new :: App Handle
new = do
  compAuxEnvRef <- asks App.compAuxEnv
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
