module Move.Context.UnusedGlobalLocator
  ( initialize,
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
import Rule.Hint
import Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedGlobalLocatorMap Map.empty

get :: App UnusedGlobalLocators
get = do
  uenv <- readRef' unusedGlobalLocatorMap
  return $ Map.toList uenv

insertIO :: IORef (Map.HashMap T.Text [(Hint, T.Text)]) -> T.Text -> Hint -> T.Text -> IO ()
insertIO ref sglText m locatorText =
  modifyIORef' ref $ Map.insertWith (++) sglText [(m, locatorText)]

deleteIO :: IORef (Map.HashMap T.Text [(Hint, T.Text)]) -> T.Text -> IO ()
deleteIO ref sglText =
  modifyIORef' ref $ Map.delete sglText
