module Context.UnusedLocalLocator
  ( initialize,
    insert,
    delete,
    registerRemarks,
    get,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark qualified as Remark
import Control.Monad
import Data.HashMap.Strict qualified as Map
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
