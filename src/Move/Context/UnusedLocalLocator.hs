module Move.Context.UnusedLocalLocator
  ( initialize,
    insert,
    delete,
    registerRemarks,
    get,
    insertIO,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Remark qualified as Remark
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.Remark
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

registerRemarks :: App ()
registerRemarks = do
  unusedLocalLocators <- get
  forM_ unusedLocalLocators $ \(ll, m) ->
    Remark.insertRemark $ newRemark m Warning $ "Imported but not used: `" <> LL.reify ll <> "`"

insertIO :: IORef (Map.HashMap LL.LocalLocator Hint) -> LL.LocalLocator -> Hint -> IO ()
insertIO ref ll m =
  modifyIORef' ref $ Map.insert ll m
