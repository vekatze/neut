module Move.UI.Handle.LocalRemark
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Data.IORef
import Rule.Remark qualified as R

newtype Handle
  = Handle
  { remarkListRef :: IORef [R.Remark] -- per file
  }

new :: IO Handle
new = do
  remarkListRef <- newIORef []
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (remarkListRef h) []

insert :: Handle -> R.Remark -> IO ()
insert h r = do
  modifyIORef' (remarkListRef h) $ (:) r

get :: Handle -> IO [R.Remark]
get h = do
  readIORef (remarkListRef h)
