module Kernel.Elaborate.Internal.Handle.WeakType
  ( Handle,
    new,
    WeakTypeEnv,
    insert,
    lookup,
    get,
  )
where

import Control.Monad.IO.Class
import Data.IORef
import Data.IntMap qualified as IntMap
import Error.EIO (EIO)
import Error.Run (raiseCritical)
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
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

get :: Handle -> IO WeakTypeEnv
get h =
  readIORef (weakTypeEnvRef h)
