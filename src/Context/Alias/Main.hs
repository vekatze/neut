module Context.Alias.Main (new) where

import qualified Context.Alias as Alias
import qualified Context.Throw as Throw
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Maybe as Maybe
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint hiding (new)
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL

type GlobalLocatorAliasMap = Map.HashMap GLA.GlobalLocatorAlias GL.GlobalLocator

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
  let additionalChecksumAlias = getAliasForThis mainModule currentModule
  Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ getModuleChecksumAliasList currentModule

getAliasForThis :: Module -> Module -> Maybe (ModuleAlias, ModuleChecksum)
getAliasForThis mainModule currentModule = do
  case MID.getModuleID mainModule currentModule of
    MID.This ->
      Nothing
    MID.That checksum ->
      return (ModuleAlias defaultModulePrefix, checksum)
    MID.Base ->
      Nothing

registerGlobalLocatorAlias ::
  Throw.Context ->
  IORef GlobalLocatorAliasMap ->
  Hint ->
  GLA.GlobalLocatorAlias ->
  GL.GlobalLocator ->
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
resolveAlias throwCtx locatorAliasMapRef moduleAliasMap m gl = do
  (moduleAlias, sourceLocator) <- resolveLocatorAlias throwCtx locatorAliasMapRef m gl
  moduleID <- resolveModuleAlias throwCtx moduleAliasMap m moduleAlias
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = moduleID,
        SGL.sourceLocator = sourceLocator
      }

resolveLocatorAlias ::
  Throw.Context ->
  IORef GlobalLocatorAliasMap ->
  Hint ->
  GL.GlobalLocator ->
  IO (ModuleAlias, SL.SourceLocator)
resolveLocatorAlias throwCtx aliasMapRef m gl = do
  aliasMap <- readIORef aliasMapRef
  case gl of
    GL.GlobalLocator moduleAlias sourceLocator ->
      return (moduleAlias, sourceLocator)
    GL.GlobalLocatorAlias alias
      | Just (GL.GlobalLocator moduleAlias sourceLocator) <- Map.lookup alias aliasMap ->
        return (moduleAlias, sourceLocator)
      | otherwise ->
        Throw.raiseError throwCtx m $
          "no such global locator alias is defined: " <> GLA.reify alias

resolveModuleAlias :: Throw.Context -> ModuleAliasMap -> Hint -> ModuleAlias -> IO MID.ModuleID
resolveModuleAlias throwCtx aliasMap m moduleAlias = do
  case Map.lookup moduleAlias aliasMap of
    Just checksum ->
      return $ MID.That checksum
    Nothing
      | moduleAlias == ModuleAlias defaultModulePrefix ->
        return MID.This
      | moduleAlias == ModuleAlias baseModulePrefix ->
        return MID.Base
      | otherwise ->
        Throw.raiseError throwCtx m $
          "no such module alias is defined: " <> extract moduleAlias

getModuleChecksumAliasList :: Module -> [(ModuleAlias, ModuleChecksum)]
getModuleChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  map (\(key, (_, checksum)) -> (key, checksum)) dependencyList
