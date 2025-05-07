module Kernel.Move.Context.Global.GlobalRemark
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Logger.Rule.Log qualified as L

newtype Handle = Handle
  { globalRemarkListRef :: IORef [L.Log]
  }

new :: IO Handle
new = do
  globalRemarkListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> [L.Log] -> IO ()
insert h remarkList = do
  modifyIORef' (globalRemarkListRef h) $ (++) remarkList

get :: Handle -> IO [L.Log]
get h = do
  readIORef (globalRemarkListRef h)
