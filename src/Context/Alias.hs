module Context.Alias
  ( resolveAlias,
    resolveLocatorAlias,
    initializeAliasMap,
    activateAliasInfo,
  )
where

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.App.Internal
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Rule.AliasInfo
import Rule.BaseName qualified as BN
import Rule.GlobalLocator qualified as GL
import Rule.GlobalLocatorAlias qualified as GLA
import Rule.Hint
import Rule.Module
import Rule.ModuleAlias
import Rule.ModuleDigest
import Rule.ModuleID qualified as MID
import Rule.Source qualified as Source
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.TopNameMap

registerGlobalLocatorAlias ::
  Hint ->
  GLA.GlobalLocatorAlias ->
  SGL.StrictGlobalLocator ->
  App ()
registerGlobalLocatorAlias m from to = do
  amap <- readRef' locatorAliasMap
  if Map.member from amap
    then Throw.raiseError m $ "The alias is already defined: " <> BN.reify (GLA.reify from)
    else modifyRef' locatorAliasMap $ Map.insert from to

resolveAlias ::
  Hint ->
  GL.GlobalLocator ->
  App SGL.StrictGlobalLocator
resolveAlias m gl = do
  case gl of
    GL.GlobalLocator (GL.IdentifiedGlobalLocator {moduleAlias, sourceLocator}) -> do
      moduleID <- resolveModuleAlias m moduleAlias
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }
    GL.GlobalLocatorAlias alias -> do
      aliasMap <- readRef' locatorAliasMap
      case Map.lookup alias aliasMap of
        Just sgl ->
          return sgl
        Nothing ->
          Throw.raiseError m $
            "No such global locator alias is defined: " <> BN.reify (GLA.reify alias)

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
        SGL.sourceLocator = sourceLocator
      }

resolveModuleAlias :: Hint -> ModuleAlias -> App MID.ModuleID
resolveModuleAlias m moduleAlias = do
  aliasMap <- readRef' moduleAliasMap
  case Map.lookup moduleAlias aliasMap of
    Just digest ->
      return $ MID.Library digest
    Nothing
      | moduleAlias == defaultModuleAlias ->
          return MID.Main
      | moduleAlias == baseModuleAlias ->
          return MID.Base
      | moduleAlias == coreModuleAlias ->
          resolveModuleAlias m defaultModuleAlias
      | otherwise ->
          Throw.raiseError m $
            "No such module alias is defined: " <> BN.reify (extract moduleAlias)

getModuleDigestAliasList :: Module -> App [(ModuleAlias, ModuleDigest)]
getModuleDigestAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  forM dependencyList $ \(key, dep) -> do
    digest' <- getLatestCompatibleDigest $ dependencyDigest dep
    return (key, digest')

getLatestCompatibleDigest :: ModuleDigest -> App ModuleDigest
getLatestCompatibleDigest mc = do
  mNewerModule <- Antecedent.lookup mc
  case mNewerModule of
    Just newerModule ->
      case moduleID newerModule of
        MID.Library newerDigest ->
          getLatestCompatibleDigest newerDigest
        _ ->
          return mc
    Nothing ->
      return mc

activateAliasInfo :: TopNameMap -> AliasInfo -> App ()
activateAliasInfo topNameMap aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      registerGlobalLocatorAlias m from to
    Use shouldUpdateTag strictGlobalLocator localLocatorList ->
      Locator.activateSpecifiedNames topNameMap shouldUpdateTag strictGlobalLocator localLocatorList

initializeAliasMap :: App ()
initializeAliasMap = do
  currentModule <- Source.sourceModule <$> readRef "currentSource" currentSource
  let additionalDigestAlias = getAlias currentModule
  currentAliasList <- getModuleDigestAliasList currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalDigestAlias] ++ currentAliasList
  writeRef' moduleAliasMap aliasMap
  writeRef' locatorAliasMap Map.empty

getAlias :: Module -> Maybe (ModuleAlias, ModuleDigest)
getAlias currentModule = do
  case moduleID currentModule of
    MID.Library digest ->
      return (defaultModuleAlias, digest)
    MID.Main ->
      Nothing
    MID.Base ->
      Nothing
