module Move.Scene.Module.GetEnabledPreset (getEnabledPreset) where

import Control.Monad.Reader (asks)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Scene.Module.GetModule qualified as Module
import Rule.BaseName qualified as BN
import Rule.Module
import Rule.ModuleAlias qualified as MA

getEnabledPreset :: Module -> App [(T.Text, [BN.BaseName])]
getEnabledPreset baseModule = do
  mainModule <- getMainModule
  counter <- asks App.counter
  mcm <- asks App.moduleCacheMap
  let h = Module.Handle {counter, mcm}
  dependencies <- toApp $ Module.getAllDependencies h mainModule baseModule
  let visibleModuleList = (MA.defaultModuleAlias, baseModule) : dependencies
  let aliasPresetInfo = map getAllTopCandidate' visibleModuleList
  let aliasList = getAliasListWithEnabledPresets baseModule
  let aliasPresetMap = constructAliasPresetMap aliasPresetInfo
  return $ concat $ flip map aliasList $ \alias -> do
    let alias' = MA.reify alias
    case Map.lookup alias' aliasPresetMap of
      Nothing ->
        []
      Just presetMap ->
        reifyPresetMap (MA.reify alias) presetMap

constructAliasPresetMap :: [(T.Text, Module)] -> AliasPresetMap
constructAliasPresetMap =
  Map.fromList . map (second modulePresetMap)

getAllTopCandidate' :: (MA.ModuleAlias, Module) -> (T.Text, Module)
getAllTopCandidate' (alias, candModule) = do
  (MA.reify alias, candModule)
