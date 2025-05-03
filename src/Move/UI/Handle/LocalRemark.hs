module Move.UI.Handle.LocalRemark
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Logger.Rule.Log qualified as L

newtype Handle
  = Handle
  { remarkListRef :: IORef [L.Log] -- per file
  }

new :: IO Handle
new = do
  remarkListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> L.Log -> IO ()
insert h r = do
  modifyIORef' (remarkListRef h) $ (:) r

get :: Handle -> IO [L.Log]
get h = do
  readIORef (remarkListRef h)
