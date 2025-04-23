module Move.Scene.Elaborate.Handle.WeakType
  ( Handle,
    new,
    insert,
    lookup,
    lookupMaybe,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseCritical)
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

newtype Handle = Handle
  { weakTypeEnvRef :: IORef (IntMap.IntMap WT.WeakTerm)
  }

new :: App Handle
new = do
  weakTypeEnvRef <- asks App.weakTypeEnv
  return $ Handle {..}

insert :: Handle -> Ident -> WT.WeakTerm -> IO ()
insert h k v =
  modifyIORef' (weakTypeEnvRef h) $ IntMap.insert (Ident.toInt k) v

lookup :: Handle -> Hint -> Ident -> EIO WT.WeakTerm
lookup h m k = do
  weakTypeEnv <- liftIO $ readIORef (weakTypeEnvRef h)
  case IntMap.lookup (Ident.toInt k) weakTypeEnv of
    Just t ->
      return t
    Nothing ->
      raiseCritical m $
        "`" <> Ident.toText' k <> "` is not found in the weak type environment."

lookupMaybe :: Handle -> Int -> IO (Maybe WT.WeakTerm)
lookupMaybe h k = do
  weakTypeEnv <- readIORef (weakTypeEnvRef h)
  return $ IntMap.lookup k weakTypeEnv
