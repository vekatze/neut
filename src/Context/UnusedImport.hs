module Context.UnusedImport
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
import Data.Text qualified as T
import Entity.Hint
import Entity.Remark
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedImportMap Map.empty

insert :: T.Text -> Hint -> T.Text -> App ()
insert sglText m locatorText =
  modifyRef' unusedImportMap $ Map.insert sglText (m, locatorText)

delete :: T.Text -> App ()
delete sglText =
  modifyRef' unusedImportMap $ Map.delete sglText

get :: App [(Hint, T.Text)]
get = do
  uenv <- readRef' unusedImportMap
  return $ Map.elems uenv

registerRemarks :: App ()
registerRemarks = do
  unusedImports <- get
  forM_ unusedImports $ \(m, locatorText) ->
    Remark.insertRemark $ newRemark m Warning $ "imported but not used: `" <> locatorText <> "`"
