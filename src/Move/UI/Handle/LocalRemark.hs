module Move.UI.Handle.LocalRemark
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Remark qualified as R

newtype Handle
  = Handle
  { remarkListRef :: IORef [R.Remark] -- per file
  }

new :: App Handle
new = do
  remarkListRef <- asks App.remarkList
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
