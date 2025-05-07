module Kernel.Move.Scene.Elaborate.Handle.LocalLogs
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Logger.Rule.Log qualified as L

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
