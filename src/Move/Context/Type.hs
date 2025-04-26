module Move.Context.Type
  ( Handle,
    new,
    initialize,
    lookupMaybe,
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

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (typeEnvRef h) Map.empty

lookupMaybe :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookupMaybe k = do
  tenv <- readRef' App.typeEnv
  return $ Map.lookup k tenv

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
