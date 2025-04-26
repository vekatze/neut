module Move.Context.UnusedLocalLocator
  ( Handle,
    new,
    initialize,
    get,
    insert,
    delete,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.UnusedLocalLocators
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { unusedLocalLocatorMapRef :: IORef (Map.HashMap LL.LocalLocator Hint)
  }

new :: App Handle
new = do
  unusedLocalLocatorMapRef <- asks App.unusedLocalLocatorMap
  return $ Handle {..}

initialize :: App ()
initialize =
  writeRef' App.unusedLocalLocatorMap Map.empty

get :: Handle -> IO UnusedLocalLocators
get h = do
  uenv <- readIORef (unusedLocalLocatorMapRef h)
  return $ Map.toList uenv

insert :: Handle -> LL.LocalLocator -> Hint -> IO ()
insert h ll m =
  modifyIORef' (unusedLocalLocatorMapRef h) $ Map.insert ll m

delete :: Handle -> LL.LocalLocator -> IO ()
delete h ll =
  modifyIORef' (unusedLocalLocatorMapRef h) $ Map.delete ll
