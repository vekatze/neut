module Move.UI.Handle.GlobalRemark
  ( Handle,
    new,
    insert,
    get,
    set,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Remark qualified as R

newtype Handle
  = Handle
  { globalRemarkListRef :: IORef [R.Remark]
  }

new :: App Handle
new = do
  globalRemarkListRef <- asks App.globalRemarkList
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
