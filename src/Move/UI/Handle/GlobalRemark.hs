module Move.UI.Handle.GlobalRemark
  ( Handle,
    new,
    insert,
    get,
    set,
  )
where

import Data.IORef
import Rule.Remark qualified as R

newtype Handle
  = Handle
  { globalRemarkListRef :: IORef [R.Remark]
  }

new :: IO Handle
new = do
  globalRemarkListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> [R.Remark] -> IO ()
insert h remarkList = do
  modifyIORef' (globalRemarkListRef h) $ (++) remarkList

get :: Handle -> IO [R.Remark]
get h = do
  readIORef (globalRemarkListRef h)

set :: Handle -> [R.Remark] -> IO ()
set h = do
  writeIORef (globalRemarkListRef h)
