module Context.Alias
  ( registerGlobalLocatorAlias,
    resolveAlias,
    resolveLocatorAlias,
    initializeAliasMap,
    activateAliasInfo,
  )
where

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.App.Internal
import Context.Locator qualified as Locator
import Context.Module qualified as Module
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Entity.AliasInfo
import Entity.BaseName qualified as BN
import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleID qualified as MID
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL

registerGlobalLocatorAlias ::
  Hint ->
  GLA.GlobalLocatorAlias ->
  SGL.StrictGlobalLocator ->
  App ()
registerGlobalLocatorAlias m from to = do
  aliasEnv <- readRef' locatorAliasMap
  if Map.member from aliasEnv
    then Throw.raiseError m $ "the global locator `" <> BN.reify (GLA.reify from) <> "` is already registered" --
    else modifyRef' locatorAliasMap $ Map.insert from to

resolveAlias ::
  Hint ->
  GL.GlobalLocator ->
  App SGL.StrictGlobalLocator
resolveAlias m gl = do
  case gl of
    GL.GlobalLocator moduleAlias sourceLocator -> do
      moduleID <- resolveModuleAlias m moduleAlias
      cgl <- Locator.getCurrentGlobalLocator
      let isPrivate = SGL.moduleID cgl == moduleID && SGL.sourceLocator cgl == sourceLocator
      return $
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator,
            SGL.isPublic = not isPrivate
          }
    GL.GlobalLocatorAlias alias -> do
      aliasMap <- readRef' locatorAliasMap
      case Map.lookup alias aliasMap of
        Just sgl ->
          return sgl
        Nothing ->
          Throw.raiseError m $
            "no such global locator alias is defined: " <> BN.reify (GLA.reify alias)

resolveLocatorAlias ::
  Hint ->
  ModuleAlias ->
  SL.SourceLocator ->
  App SGL.StrictGlobalLocator
resolveLocatorAlias m moduleAlias sourceLocator = do
  moduleID <- resolveModuleAlias m moduleAlias
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = moduleID,
        SGL.sourceLocator = sourceLocator,
        SGL.isPublic = True
      }

resolveModuleAlias :: Hint -> ModuleAlias -> App MID.ModuleID
resolveModuleAlias m moduleAlias = do
  aliasMap <- readRef' moduleAliasMap
  case Map.lookup moduleAlias aliasMap of
    Just checksum ->
      return $ MID.Library checksum
    Nothing
      | moduleAlias == defaultModulePrefix ->
          return MID.Main
      | moduleAlias == baseModulePrefix ->
          return MID.Base
      | moduleAlias == coreModulePrefix ->
          resolveModuleAlias m defaultModulePrefix
      | otherwise ->
          Throw.raiseError m $
            "no such module alias is defined: " <> BN.reify (extract moduleAlias)

getModuleChecksumAliasList :: Module -> App [(ModuleAlias, ModuleChecksum)]
getModuleChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  forM dependencyList $ \(key, (_, checksum)) -> do
    checksum' <- getLatestCompatibleChecksum checksum
    return (key, checksum')

getLatestCompatibleChecksum :: ModuleChecksum -> App ModuleChecksum
getLatestCompatibleChecksum mc = do
  mNewerModule <- Antecedent.lookup mc
  case mNewerModule of
    Just newerModule ->
      case moduleID newerModule of
        MID.Library newerChecksum ->
          getLatestCompatibleChecksum newerChecksum
        _ ->
          return mc
    Nothing ->
      return mc

activateAliasInfo :: AliasInfo -> App ()
activateAliasInfo aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      registerGlobalLocatorAlias m from to
    Use strictGlobalLocator ->
      Locator.activateGlobalLocator strictGlobalLocator

initializeAliasMap :: App ()
initializeAliasMap = do
  currentModule <- Source.sourceModule <$> readRef "currentSource" currentSource
  mainModule <- Module.getMainModule
  let additionalChecksumAlias = getAlias mainModule currentModule
  currentAliasList <- getModuleChecksumAliasList currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ currentAliasList
  writeRef' moduleAliasMap aliasMap
  writeRef' locatorAliasMap Map.empty

getAlias :: Module -> Module -> Maybe (ModuleAlias, ModuleChecksum)
getAlias mainModule currentModule = do
  case getID mainModule currentModule of
    MID.Library checksum ->
      return (defaultModulePrefix, checksum)
    MID.Main ->
      Nothing
    MID.Base ->
      Nothing
