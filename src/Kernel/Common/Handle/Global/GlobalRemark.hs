module Kernel.Common.Handle.Global.GlobalRemark
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Kernel.Common.RuleHandle.Global.GlobalRemark
import Logger.Log qualified as L

new :: IO Handle
new = do
  _globalRemarkListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> [L.Log] -> IO ()
insert h remarkList = do
  modifyIORef' (_globalRemarkListRef h) $ (++) remarkList

get :: Handle -> IO [L.Log]
get h = do
  readIORef (_globalRemarkListRef h)
