module Move.Context.UnusedPreset
  ( initialize,
    insert,
    deleteIO,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Rule.Hint
import Prelude hiding (lookup, read)

type ModuleIDText =
  T.Text

initialize :: App ()
initialize =
  writeRef' unusedPresetMap Map.empty

insert :: ModuleIDText -> Hint -> App ()
insert presetName m =
  modifyRef' unusedPresetMap $ Map.insert presetName m

deleteIO :: IORef (Map.HashMap T.Text Hint) -> ModuleIDText -> IO ()
deleteIO ref presetName =
  modifyIORef' ref $ Map.delete presetName
