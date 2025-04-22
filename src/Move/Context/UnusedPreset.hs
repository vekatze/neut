module Move.Context.UnusedPreset
  ( initialize,
    insert,
    delete,
    registerRemarks,
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
import Prelude hiding (lookup, read)

type ModuleIDText =
  T.Text

initialize :: App ()
initialize =
  writeRef' unusedPresetMap Map.empty

insert :: ModuleIDText -> Hint -> App ()
insert presetName m =
  modifyRef' unusedPresetMap $ Map.insert presetName m

delete :: ModuleIDText -> App ()
delete presetName =
  modifyRef' unusedPresetMap $ Map.delete presetName

get :: App [(ModuleIDText, Hint)]
get = do
  uenv <- readRef' unusedPresetMap
  return $ Map.toList uenv

registerRemarks :: App ()
registerRemarks = do
  unusedPresets <- get
  forM_ unusedPresets $ \(presetName, m) ->
    Remark.insertRemark $ newRemark m Warning $ "Imported but not used: `" <> presetName <> "`"

deleteIO :: IORef (Map.HashMap T.Text Hint) -> ModuleIDText -> IO ()
deleteIO ref presetName =
  modifyIORef' ref $ Map.delete presetName
