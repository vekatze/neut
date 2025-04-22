module Move.Scene.Module.GetEnabledPreset
  ( Handle,
    new,
    getEnabledPreset,
  )
where

import Control.Monad.Reader (asks)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO)
import Move.Context.Env (getMainModule)
import Move.Scene.Module.GetModule qualified as Module
import Path
import Rule.BaseName qualified as BN
import Rule.Module
import Rule.ModuleAlias qualified as MA

data Handle
  = Handle
  { counter :: IORef Int,
    moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module),
    mainModule :: MainModule
  }

new :: App Handle
new = do
  counter <- asks App.counter
  moduleCacheMapRef <- asks App.moduleCacheMap
  mainModule <- getMainModule
  return $ Handle {..}

getEnabledPreset :: Handle -> Module -> EIO [(T.Text, [BN.BaseName])]
getEnabledPreset h baseModule = do
  let h' = Module.Handle {counter = counter h, mcm = moduleCacheMapRef h}
  dependencies <- Module.getAllDependencies h' (mainModule h) baseModule
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
