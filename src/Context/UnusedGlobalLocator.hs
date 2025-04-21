module Context.UnusedGlobalLocator
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
import Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedGlobalLocatorMap Map.empty

insert :: T.Text -> Hint -> T.Text -> App ()
insert sglText m locatorText =
  modifyRef' unusedGlobalLocatorMap $ Map.insertWith (++) sglText [(m, locatorText)]

delete :: T.Text -> App ()
delete sglText =
  modifyRef' unusedGlobalLocatorMap $ Map.delete sglText

get :: App UnusedGlobalLocators
get = do
  uenv <- readRef' unusedGlobalLocatorMap
  return $ Map.toList uenv

registerRemarks :: App ()
registerRemarks = do
  unusedGlobalLocators <- concatMap snd <$> get
  forM_ unusedGlobalLocators $ \(m, locatorText) ->
    Remark.insertRemark $ newRemark m Warning $ "Imported but not used: `" <> locatorText <> "`"
