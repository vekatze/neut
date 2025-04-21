module Move.Context.UnusedPreset
  ( initialize,
    insert,
    delete,
    registerRemarks,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Remark qualified as Remark
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
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
