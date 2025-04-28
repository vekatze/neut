module Move.Context.Alias
  ( Handle,
    new,
    resolveAlias,
    resolveLocatorAlias,
    initializeAliasMap,
    activateAliasInfo,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe qualified as Maybe
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
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

data Handle
  = Handle
  { antecedentHandle :: Antecedent.Handle,
    locatorHandle :: Locator.Handle,
    envHandle :: Env.Handle,
    locatorAliasMapRef :: IORef (Map.HashMap GLA.GlobalLocatorAlias SGL.StrictGlobalLocator),
    moduleAliasMapRef :: IORef (Map.HashMap ModuleAlias ModuleDigest)
  }

new :: Env.Handle -> App Handle
new envHandle = do
  antecedentHandle <- Antecedent.new
  locatorHandle <- Locator.new envHandle
  locatorAliasMapRef <- asks App.locatorAliasMap
  moduleAliasMapRef <- asks App.moduleAliasMap
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

getModuleDigestAliasList :: Handle -> Module -> EIO [(ModuleAlias, ModuleDigest)]
getModuleDigestAliasList h baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  forM dependencyList $ \(key, dep) -> do
    digest' <- getLatestCompatibleDigest h $ dependencyDigest dep
    return (key, digest')

getLatestCompatibleDigest :: Handle -> ModuleDigest -> EIO ModuleDigest
getLatestCompatibleDigest h mc = do
  antecedentMap <- liftIO $ Antecedent.get (antecedentHandle h)
  case Map.lookup (MID.Library mc) antecedentMap of
    Just newerModule ->
      case moduleID newerModule of
        MID.Library newerDigest ->
          getLatestCompatibleDigest h newerDigest
        _ ->
          return mc
    Nothing ->
      return mc

activateAliasInfo :: Handle -> TopNameMap -> AliasInfo -> EIO ()
activateAliasInfo h topNameMap aliasInfo =
  case aliasInfo of
    Prefix m from to ->
      registerGlobalLocatorAlias h m from to
    Use shouldUpdateTag strictGlobalLocator localLocatorList ->
      Locator.activateSpecifiedNames (locatorHandle h) topNameMap shouldUpdateTag strictGlobalLocator localLocatorList

initializeAliasMap :: Handle -> EIO ()
initializeAliasMap h = do
  currentModule <- Source.sourceModule <$> Env.getCurrentSource (envHandle h)
  let additionalDigestAlias = getAlias currentModule
  currentAliasList <- getModuleDigestAliasList h currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalDigestAlias] ++ currentAliasList
  liftIO $ writeIORef (moduleAliasMapRef h) aliasMap
  liftIO $ writeIORef (locatorAliasMapRef h) Map.empty

getAlias :: Module -> Maybe (ModuleAlias, ModuleDigest)
getAlias currentModule = do
  case moduleID currentModule of
    MID.Library digest ->
      return (defaultModuleAlias, digest)
    MID.Main ->
      Nothing
    MID.Base ->
      Nothing
