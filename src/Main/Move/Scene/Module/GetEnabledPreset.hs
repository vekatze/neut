module Main.Move.Scene.Module.GetEnabledPreset
  ( Handle,
    new,
    getEnabledPreset,
  )
where

import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Module qualified as Module
import Main.Move.Language.Utility.Gensym qualified as Gensym
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.GetModule qualified as GetModule
import Main.Rule.BaseName qualified as BN
import Main.Rule.Module
import Main.Rule.ModuleAlias qualified as MA

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    moduleHandle :: Module.Handle,
    envHandle :: Env.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  Handle {..}

getEnabledPreset :: Handle -> Module -> EIO [(T.Text, [BN.BaseName])]
getEnabledPreset h baseModule = do
  let h' = GetModule.Handle {gensymHandle = gensymHandle h, moduleHandle = moduleHandle h}
  let mainModule = Env.getMainModule (envHandle h)
  dependencies <- GetModule.getAllDependencies h' mainModule baseModule
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
