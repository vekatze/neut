module Kernel.Common.Module.GetEnabledPreset
  ( Handle,
    new,
    getEnabledPreset,
  )
where

import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Error.EIO (EIO)
import Gensym.Handle qualified as Gensym
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Module
import Kernel.Common.Module.GetModule qualified as GetModule
import Language.Common.BaseName qualified as BN
import Language.Common.ModuleAlias qualified as MA

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    moduleHandle :: Module.Handle,
    envHandle :: Env.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
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
