module Kernel.Elaborate.Move.Internal.Handle.LocalLogs
  ( Handle,
    new,
    insert,
    get,
  )
where

import Logger.Rule.Log qualified as L
import Data.IORef

newtype Handle = Handle
  { logsRef :: IORef [L.Log]
  }

new :: IO Handle
new = do
  logsRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> L.Log -> IO ()
insert h r = do
  modifyIORef' (logsRef h) $ (:) r

get :: Handle -> IO [L.Log]
get h = do
  readIORef (logsRef h)
