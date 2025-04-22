module Move.Context.UnusedGlobalLocator
  ( initialize,
    insert,
    delete,
    registerRemarks,
    get,
    insertIO,
    deleteIO,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Remark qualified as Remark
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

insertIO :: IORef (Map.HashMap T.Text [(Hint, T.Text)]) -> T.Text -> Hint -> T.Text -> IO ()
insertIO ref sglText m locatorText =
  modifyIORef' ref $ Map.insertWith (++) sglText [(m, locatorText)]

deleteIO :: IORef (Map.HashMap T.Text [(Hint, T.Text)]) -> T.Text -> IO ()
deleteIO ref sglText =
  modifyIORef' ref $ Map.delete sglText
