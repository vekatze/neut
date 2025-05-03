module Main.Move.Scene.Elaborate.Handle.WeakType
  ( Handle,
    new,
    WeakTypeEnv,
    insert,
    lookup,
    lookupMaybe,
    get,
  )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.IntMap qualified as IntMap
import Main.Move.Context.EIO (EIO, raiseCritical)
import Main.Rule.Hint
import Main.Rule.Ident
import Main.Rule.Ident.Reify qualified as Ident
import Main.Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

type WeakTypeEnv =
  IntMap.IntMap WT.WeakTerm

newtype Handle = Handle
  { weakTypeEnvRef :: IORef WeakTypeEnv
  }

new :: IO Handle
new = do
  weakTypeEnvRef <- newIORef IntMap.empty
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

get :: Handle -> IO WeakTypeEnv
get h =
  readIORef (weakTypeEnvRef h)
