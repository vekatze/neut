module Main.Move.Context.GlobalRemark
  ( Handle,
    new,
    insert,
    get,
    set,
  )
where

import Data.IORef
import Logger.Rule.Log qualified as L

newtype Handle
  = Handle
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

set :: Handle -> [L.Log] -> IO ()
set h = do
  writeIORef (globalRemarkListRef h)
