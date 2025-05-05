module Main.Move.Scene.Parse.Handle.Alias
  ( Handle,
    new,
    resolveAlias,
    resolveLocatorAlias,
    activateAliasInfo,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe qualified as Maybe
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.GlobalLocator qualified as GL
import Language.Common.Rule.GlobalLocatorAlias qualified as GLA
import Language.Common.Rule.Hint
import Language.Common.Rule.ModuleAlias
import Language.Common.Rule.ModuleDigest
import Language.Common.Rule.ModuleID qualified as MID
import Language.Common.Rule.SourceLocator qualified as SL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Main.Move.Context.Antecedent qualified as Antecedent
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Locator qualified as Locator
import Main.Rule.AliasInfo
import Main.Rule.Module
import Main.Rule.Source qualified as Source
import Main.Rule.TopNameMap

data Handle = Handle
  { antecedentHandle :: Antecedent.Handle,
    locatorHandle :: Locator.Handle,
    envHandle :: Env.Handle,
    locatorAliasMapRef :: IORef (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator),
    moduleAliasMapRef :: IORef (Map.HashMap ModuleAlias ModuleDigest)
  }

new :: Antecedent.Handle -> Locator.Handle -> Env.Handle -> Source.Source -> IO Handle
new antecedentHandle locatorHandle envHandle source = do
  let currentModule = Source.sourceModule source
  let additionalDigestAlias = getAlias currentModule
  currentAliasList <- getModuleDigestAliasList antecedentHandle currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalDigestAlias] ++ currentAliasList
  moduleAliasMapRef <- newIORef aliasMap
  locatorAliasMapRef <- newIORef Map.empty
  return $ Handle {..}

registerGlobalLocatorAlias ::
  Handle ->
  Hint ->
  GLA.GlobalLocatorAlias ->
  SGL.StrictGlobalLocator ->
  EIO ()
registerGlobalLocatorAlias h m from to = do
  locatorAliasMap <- liftIO $ readIORef (locatorAliasMapRef h)
  if Map.member from locatorAliasMap
    then raiseError m $ "The alias is already defined: " <> BN.reify (GLA.reify from)
    else liftIO $ modifyIORef' (locatorAliasMapRef h) $ Map.insert from to

resolveAlias ::
  Handle ->
  Hint ->
  GL.GlobalLocator ->
  EIO SGL.StrictGlobalLocator
resolveAlias h m gl = do
  case gl of
    GL.GlobalLocator (GL.IdentifiedGlobalLocator {moduleAlias, sourceLocator}) -> do
      moduleID <- resolveModuleAlias h m moduleAlias
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }
    GL.GlobalLocatorAlias alias -> do
      aliasMap <- liftIO $ readIORef (locatorAliasMapRef h)
      case Map.lookup alias aliasMap of
        Just sgl ->
          return sgl
        Nothing ->
          raiseError m $
            "No such global locator alias is defined: " <> BN.reify (GLA.reify alias)

resolveLocatorAlias ::
  Handle ->
  Hint ->
  ModuleAlias ->
  SL.SourceLocator ->
  EIO SGL.StrictGlobalLocator
resolveLocatorAlias h m moduleAlias sourceLocator = do
  moduleID <- resolveModuleAlias h m moduleAlias
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = moduleID,
        SGL.sourceLocator = sourceLocator
      }

resolveModuleAlias :: Handle -> Hint -> ModuleAlias -> EIO MID.ModuleID
resolveModuleAlias h m moduleAlias = do
  aliasMap <- liftIO $ readIORef (moduleAliasMapRef h)
  case Map.lookup moduleAlias aliasMap of
    Just digest ->
      return $ MID.Library digest
    Nothing
      | moduleAlias == defaultModuleAlias ->
          return MID.Main
      | moduleAlias == baseModuleAlias ->
          return MID.Base
      | moduleAlias == coreModuleAlias ->
          resolveModuleAlias h m defaultModuleAlias
      | otherwise ->
          raiseError m $
            "No such module alias is defined: " <> BN.reify (extract moduleAlias)

getModuleDigestAliasList :: Antecedent.Handle -> Module -> IO [(ModuleAlias, ModuleDigest)]
getModuleDigestAliasList h baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  forM dependencyList $ \(key, dep) -> do
    digest' <- getLatestCompatibleDigest h $ dependencyDigest dep
    return (key, digest')

getLatestCompatibleDigest :: Antecedent.Handle -> ModuleDigest -> IO ModuleDigest
getLatestCompatibleDigest h mc = do
  antecedentMap <- Antecedent.get h
  case Map.lookup (MID.Library mc) antecedentMap of
    Just newerModule ->
      case moduleID newerModule of
        MID.Library newerDigest ->
          getLatestCompatibleDigest h newerDigest
        _ ->
          return mc
    Nothing ->
      return mc

activateAliasInfo :: Handle -> Source.Source -> TopNameMap -> AliasInfo -> EIO ()
activateAliasInfo h source topNameMap aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      registerGlobalLocatorAlias h m from to
    Use shouldUpdateTag strictGlobalLocator localLocatorList ->
      Locator.activateSpecifiedNames (locatorHandle h) source topNameMap shouldUpdateTag strictGlobalLocator localLocatorList

getAlias :: Module -> Maybe (ModuleAlias, ModuleDigest)
getAlias currentModule = do
  case moduleID currentModule of
    MID.Library digest ->
      return (defaultModuleAlias, digest)
    MID.Main ->
      Nothing
    MID.Base ->
      Nothing
