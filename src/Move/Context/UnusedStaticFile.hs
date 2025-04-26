module Move.Context.UnusedStaticFile
  ( Handle,
    new,
    initialize,
    delete,
    get,
    insert,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { unusedStaticFileMapRef :: IORef (Map.HashMap T.Text Hint)
  }

new :: App Handle
new = do
  unusedStaticFileMapRef <- asks App.unusedStaticFileMap
  return $ Handle {..}

initialize :: App ()
initialize =
  writeRef' App.unusedStaticFileMap Map.empty

delete :: Handle -> T.Text -> IO ()
delete h ll =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.delete ll

get :: Handle -> IO [(T.Text, Hint)]
get h = do
  uenv <- readIORef (unusedStaticFileMapRef h)
  return $ Map.toList uenv

insert :: Handle -> T.Text -> Hint -> IO ()
insert h ll m =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.insert ll m
