module Case.Main.Alias
  ( registerGlobalLocatorAlias,
    resolveAlias,
    initializeAliasMap,
    Context,
  )
where

import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Entity.BaseName qualified as BN
import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint hiding (new)
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleID qualified as MID
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m
  ) =>
  Context m

initializeAliasMap :: Context m => m ()
initializeAliasMap = do
  currentModule <- Source.sourceModule <$> Env.getCurrentSource
  mainModule <- Env.getMainModule
  let additionalChecksumAlias = getAlias mainModule currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ getModuleChecksumAliasList currentModule
  Env.setModuleAliasMap aliasMap

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
  Context m =>
  Hint ->
  GLA.GlobalLocatorAlias ->
  SGL.StrictGlobalLocator ->
  m ()
registerGlobalLocatorAlias m from to = do
  aliasEnv <- Env.getLocatorAliasMap
  if Map.member from aliasEnv
    then Throw.raiseError m $ "the global locator `" <> BN.reify (GLA.reify from) <> "` is already registered"
    else Env.insertToLocatorAliasMap from to

resolveAlias ::
  Context m =>
  Hint ->
  GL.GlobalLocator ->
  m SGL.StrictGlobalLocator
resolveAlias m gl = do
  case gl of
    GL.GlobalLocator moduleAlias sourceLocator -> do
      moduleID <- resolveModuleAlias m moduleAlias
      return $
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }
    GL.GlobalLocatorAlias alias -> do
      aliasMap <- Env.getLocatorAliasMap
      case Map.lookup alias aliasMap of
        Just sgl ->
          return sgl
        Nothing ->
          Throw.raiseError m $
            "no such global locator alias is defined: " <> BN.reify (GLA.reify alias)

resolveModuleAlias :: Context m => Hint -> ModuleAlias -> m MID.ModuleID
resolveModuleAlias m moduleAlias = do
  aliasMap <- Env.getModuleAliasMap
  case Map.lookup moduleAlias aliasMap of
    Just checksum ->
      return $ MID.Library checksum
    Nothing
      | moduleAlias == defaultModulePrefix ->
          return MID.Main
      | moduleAlias == baseModulePrefix ->
          return MID.Base
      | otherwise ->
          Throw.raiseError m $
            "no such module alias is defined: " <> BN.reify (extract moduleAlias)

getModuleChecksumAliasList :: Module -> [(ModuleAlias, ModuleChecksum)]
getModuleChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  map (\(key, (_, checksum)) -> (key, checksum)) dependencyList
