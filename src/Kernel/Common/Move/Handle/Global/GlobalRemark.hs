module Kernel.Common.Move.Handle.Global.GlobalRemark
  ( new,
    insert,
    get,
  )
where

import Aux.Logger.Rule.Log qualified as L
import Data.IORef
import Kernel.Common.Rule.Handle.Global.GlobalRemark

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
