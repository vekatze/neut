module Context.UnusedStaticFile
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
import Data.Text qualified as T
import Rule.Hint
import Rule.Remark
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedStaticFileMap Map.empty

insert :: T.Text -> Hint -> App ()
insert ll m =
  modifyRef' unusedStaticFileMap $ Map.insert ll m

delete :: T.Text -> App ()
delete ll =
  modifyRef' unusedStaticFileMap $ Map.delete ll

get :: App [(T.Text, Hint)]
get = do
  uenv <- readRef' unusedStaticFileMap
  return $ Map.toList uenv

registerRemarks :: App ()
registerRemarks = do
  unusedStaticFiles <- get
  forM_ unusedStaticFiles $ \(k, m) ->
    Remark.insertRemark $ newRemark m Warning $ "Imported but not used: `" <> k <> "`"
