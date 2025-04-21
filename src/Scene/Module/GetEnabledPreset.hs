module Scene.Module.GetEnabledPreset (getEnabledPreset) where

import Context.App
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Rule.BaseName qualified as BN
import Rule.Module
import Rule.ModuleAlias qualified as MA
import Scene.Module.Reflect qualified as Module

getEnabledPreset :: Module -> App [(T.Text, [BN.BaseName])]
getEnabledPreset baseModule = do
  dependencies <- Module.getAllDependencies baseModule
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
