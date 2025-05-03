module Main.Move.Context.Type
  ( Handle,
    new,
    insert',
    lookup',
    lookupMaybe',
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Main.Move.Context.EIO (EIO, raiseCritical)
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Hint
import Main.Rule.WeakTerm
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
  }

new :: IO Handle
new = do
  typeEnvRef <- newIORef Map.empty
  return $ Handle {..}

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
