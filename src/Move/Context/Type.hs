module Move.Context.Type
  ( Handle,
    new,
    initialize,
    lookup,
    lookupMaybe,
    insert,
    insert',
    lookup',
    lookupMaybe',
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseCritical)
import Move.Context.Throw qualified as Throw
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.WeakTerm
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
  }

new :: App Handle
new = do
  typeEnvRef <- asks App.typeEnv
  return $ Handle {..}

initialize :: App ()
initialize = do
  writeRef' App.typeEnv Map.empty

lookup :: Hint -> DD.DefiniteDescription -> App WeakTerm
lookup m k = do
  valueOrNone <- lookupMaybe k
  case valueOrNone of
    Just value ->
      return value
    Nothing ->
      Throw.raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

lookupMaybe :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookupMaybe k = do
  tenv <- readRef' App.typeEnv
  return $ Map.lookup k tenv

insert :: DD.DefiniteDescription -> WeakTerm -> App ()
insert k v =
  modifyRef' App.typeEnv $ Map.insert k v

insert' :: Handle -> DD.DefiniteDescription -> WeakTerm -> IO ()
insert' h k v =
  modifyIORef' (typeEnvRef h) $ Map.insert k v

lookup' :: Handle -> Hint -> DD.DefiniteDescription -> EIO WeakTerm
lookup' h m k = do
  valueOrNone <- liftIO $ lookupMaybe' h k
  case valueOrNone of
    Just value ->
      return value
    Nothing ->
      raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

lookupMaybe' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakTerm)
lookupMaybe' h k = do
  typeEnv <- readIORef (typeEnvRef h)
  return $ Map.lookup k typeEnv
