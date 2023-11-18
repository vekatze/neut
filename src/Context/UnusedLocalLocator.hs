module Context.UnusedLocalLocator
  ( initialize,
    insert,
    delete,
    registerRemarks,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark qualified as Remark
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Remark
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

get :: App [(LL.LocalLocator, Hint)]
get = do
  uenv <- readRef' unusedLocalLocatorMap
  return $ Map.toList uenv

registerRemarks :: App ()
registerRemarks = do
  unusedLocalLocators <- get
  forM_ unusedLocalLocators $ \(ll, m) ->
    Remark.insertRemark $ newRemark m Warning $ "imported but not used: `" <> LL.reify ll <> "`"
