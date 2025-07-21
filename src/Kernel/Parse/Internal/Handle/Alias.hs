module Kernel.Parse.Internal.Handle.Alias
  ( Handle,
    new,
    resolveAlias,
    resolveLocatorAlias,
    activateAliasInfo,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe qualified as Maybe
import Kernel.Common.AliasInfo
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Module
import Kernel.Common.Source qualified as Source
import Kernel.Common.TopNameMap
import Language.Common.BaseName qualified as BN
import Language.Common.GlobalLocator qualified as GL
import Language.Common.ModuleAlias
import Language.Common.ModuleDigest
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint

data Handle = Handle
  { antecedentHandle :: Antecedent.Handle,
    locatorHandle :: Locator.Handle,
    envHandle :: Env.Handle,
    moduleAliasMapRef :: IORef (Map.HashMap ModuleAlias ModuleDigest)
  }

new :: Antecedent.Handle -> Locator.Handle -> Env.Handle -> Source.Source -> IO Handle
new antecedentHandle locatorHandle envHandle source = do
  let currentModule = Source.sourceModule source
  let additionalDigestAlias = getAlias currentModule
  currentAliasList <- getModuleDigestAliasList antecedentHandle currentModule
  let aliasMap = Map.fromList $ Maybe.catMaybes [additionalDigestAlias] ++ currentAliasList
  moduleAliasMapRef <- newIORef aliasMap
  return $ Handle {..}

resolveAlias ::
  Handle ->
  Hint ->
  GL.GlobalLocator ->
  App SGL.StrictGlobalLocator
resolveAlias h m gl = do
  case gl of
    GL.GlobalLocator (GL.IdentifiedGlobalLocator {moduleAlias, sourceLocator}) -> do
      moduleID <- resolveModuleAlias h m moduleAlias
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = sourceLocator
          }

resolveLocatorAlias ::
  Handle ->
  Hint ->
  ModuleAlias ->
  SL.SourceLocator ->
  App SGL.StrictGlobalLocator
resolveLocatorAlias h m moduleAlias sourceLocator = do
  moduleID <- resolveModuleAlias h m moduleAlias
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = moduleID,
        SGL.sourceLocator = sourceLocator
      }

resolveModuleAlias :: Handle -> Hint -> ModuleAlias -> App MID.ModuleID
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

activateAliasInfo :: Handle -> Source.Source -> TopNameMap -> AliasInfo -> App ()
activateAliasInfo h source topNameMap aliasInfo =
  case aliasInfo of
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
