module Move.Scene.Module.GetEnabledPreset
  ( Handle,
    new,
    getEnabledPreset,
  )
where

import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env (getMainModule)
import Move.Context.Module qualified as Module
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Module.GetModule qualified as GetModule
import Rule.BaseName qualified as BN
import Rule.Module
import Rule.ModuleAlias qualified as MA

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    moduleHandle :: Module.Handle,
    mainModule :: MainModule
  }

new :: App Handle
new = do
  gensymHandle <- Gensym.new
  moduleHandle <- Module.new
  mainModule <- getMainModule
  return $ Handle {..}

getEnabledPreset :: Handle -> Module -> EIO [(T.Text, [BN.BaseName])]
getEnabledPreset h baseModule = do
  let h' = GetModule.Handle {gensymHandle = gensymHandle h, moduleHandle = moduleHandle h}
  dependencies <- GetModule.getAllDependencies h' (mainModule h) baseModule
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
