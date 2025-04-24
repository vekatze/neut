module Move.Context.UnusedLocalLocator
  ( initialize,
    insert,
    delete,
    get,
    insertIO,
    deleteIO,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.UnusedLocalLocators
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedLocalLocatorMap Map.empty

insert :: LL.LocalLocator -> Hint -> App ()
insert ll m =
  modifyRef' unusedLocalLocatorMap $ Map.insert ll m

delete :: LL.LocalLocator -> App ()
delete ll =
  modifyRef' unusedLocalLocatorMap $ Map.delete ll

get :: App UnusedLocalLocators
get = do
  uenv <- readRef' unusedLocalLocatorMap
  return $ Map.toList uenv

insertIO :: IORef (Map.HashMap LL.LocalLocator Hint) -> LL.LocalLocator -> Hint -> IO ()
insertIO ref ll m =
  modifyIORef' ref $ Map.insert ll m

deleteIO :: IORef (Map.HashMap LL.LocalLocator Hint) -> LL.LocalLocator -> IO ()
deleteIO ref ll =
  modifyIORef' ref $ Map.delete ll
