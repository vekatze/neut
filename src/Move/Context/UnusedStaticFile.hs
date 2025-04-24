module Move.Context.UnusedStaticFile
  ( initialize,
    insert,
    delete,
    get,
    insertIO,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Rule.Hint
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

insertIO :: IORef (Map.HashMap T.Text Hint) -> T.Text -> Hint -> IO ()
insertIO ref ll m =
  modifyIORef' ref $ Map.insert ll m
