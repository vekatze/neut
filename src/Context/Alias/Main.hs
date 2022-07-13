module Context.Alias.Main (new) where

import qualified Context.Alias as Alias
import qualified Context.Throw as Throw
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Maybe as Maybe
import Entity.Const
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint hiding (new)
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
import qualified Entity.StrictGlobalLocator as SGL

type GlobalLocatorAliasMap = Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator

type ModuleAliasMap = Map.HashMap ModuleAlias ModuleChecksum

new :: Alias.Config -> IO Alias.Context
new cfg = do
  locatorAliasMapRef <- newIORef Map.empty
  let moduleAliasMap = createModuleAliasMap (Alias.currentModule cfg) (Alias.mainModule cfg)
  return
    Alias.Context
      { Alias.registerGlobalLocatorAlias =
          registerGlobalLocatorAlias (Alias.throwCtx cfg) locatorAliasMapRef,
        Alias.resolveAlias =
          resolveAlias (Alias.throwCtx cfg) locatorAliasMapRef moduleAliasMap
      }

createModuleAliasMap :: Module -> Module -> ModuleAliasMap
createModuleAliasMap currentModule mainModule = do
  let additionalChecksumAlias = getAlias mainModule currentModule
  Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ getModuleChecksumAliasList currentModule

getAlias :: Module -> Module -> Maybe (ModuleAlias, ModuleChecksum)
getAlias mainModule currentModule = do
  case getID mainModule currentModule of
    MID.Library checksum ->
      return (defaultModulePrefix, checksum)
    MID.Main ->
      Nothing
    MID.Base ->
      Nothing

registerGlobalLocatorAlias ::
  Throw.Context ->
  IORef GlobalLocatorAliasMap ->
  Hint ->
  GLA.GlobalLocatorAlias ->
  SGL.StrictGlobalLocator ->
  IO ()
registerGlobalLocatorAlias ctx locatorAliasMapRef m from to = do
  aliasEnv <- readIORef locatorAliasMapRef
  if Map.member from aliasEnv
    then Throw.raiseError ctx m $ "the global locator `" <> GLA.reify from <> "` is already registered"
    else writeIORef locatorAliasMapRef $ Map.insert from to aliasEnv

resolveAlias ::
  Throw.Context ->
  IORef GlobalLocatorAliasMap ->
  ModuleAliasMap ->
  Hint ->
  GL.GlobalLocator ->
  IO SGL.StrictGlobalLocator
resolveAlias throwCtx aliasMapRef moduleAliasMap m gl = do
  case gl of
    GL.GlobalLocator moduleAlias sourceLocator -> do
      moduleID <- resolveModuleAlias throwCtx moduleAliasMap m moduleAlias
      return $
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }
    GL.GlobalLocatorAlias alias -> do
      aliasMap <- readIORef aliasMapRef
      case Map.lookup alias aliasMap of
        Just sgl ->
          return sgl
        Nothing ->
          Throw.raiseError throwCtx m $
            "no such global locator alias is defined: " <> GLA.reify alias

resolveModuleAlias :: Throw.Context -> ModuleAliasMap -> Hint -> ModuleAlias -> IO MID.ModuleID
resolveModuleAlias throwCtx aliasMap m moduleAlias = do
  case Map.lookup moduleAlias aliasMap of
    Just checksum ->
      return $ MID.Library checksum
    Nothing
      | moduleAlias == defaultModulePrefix ->
        return MID.Main
      | moduleAlias == baseModulePrefix ->
        return MID.Base
      | otherwise ->
        Throw.raiseError throwCtx m $
          "no such module alias is defined: " <> extract moduleAlias

getModuleChecksumAliasList :: Module -> [(ModuleAlias, ModuleChecksum)]
getModuleChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  map (\(key, (_, checksum)) -> (key, checksum)) dependencyList
