module Move.Context.UnusedGlobalLocator
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
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { unusedGlobalLocatorMapRef :: IORef (Map.HashMap T.Text [(Hint, T.Text)]) -- (SGL ~> [(hint, locatorText)])
  }

new :: App Handle
new = do
  unusedGlobalLocatorMapRef <- asks App.unusedGlobalLocatorMap
  return $ Handle {..}

initialize :: App ()
initialize =
  writeRef' App.unusedGlobalLocatorMap Map.empty

get :: Handle -> IO UnusedGlobalLocators
get h = do
  uenv <- readIORef (unusedGlobalLocatorMapRef h)
  return $ Map.toList uenv

insert :: Handle -> T.Text -> Hint -> T.Text -> IO ()
insert h sglText m locatorText =
  modifyIORef' (unusedGlobalLocatorMapRef h) $ Map.insertWith (++) sglText [(m, locatorText)]

delete :: Handle -> T.Text -> IO ()
delete h sglText =
  modifyIORef' (unusedGlobalLocatorMapRef h) $ Map.delete sglText
