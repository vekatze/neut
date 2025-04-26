module Move.Context.UnusedPreset
  ( Handle,
    new,
    initialize,
    insert,
    delete,
    get,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Prelude hiding (lookup, read)

type ModuleIDText =
  T.Text

newtype Handle
  = Handle
  { unusedPresetMapRef :: IORef (Map.HashMap T.Text Hint) -- (ModuleID ~> Hint)
  }

new :: App Handle
new = do
  unusedPresetMapRef <- asks App.unusedPresetMap
  return $ Handle {..}

initialize :: App ()
initialize =
  writeRef' App.unusedPresetMap Map.empty

insert :: Handle -> ModuleIDText -> Hint -> IO ()
insert h presetName m =
  modifyIORef' (unusedPresetMapRef h) $ Map.insert presetName m

delete :: Handle -> ModuleIDText -> IO ()
delete h presetName =
  modifyIORef' (unusedPresetMapRef h) $ Map.delete presetName

get :: Handle -> IO (Map.HashMap T.Text Hint)
get h =
  readIORef (unusedPresetMapRef h)
