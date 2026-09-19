module Kernel.Common.CreateGlobalHandle
  ( Handle (..),
    new,
    newOrError,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (runApp)
import Console.Handle qualified as Console
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, newIORef)
import Data.Set qualified as S
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Expose qualified as Expose
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.ImportedTypeDefCache qualified as ImportedTypeDefCache
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Resource qualified as Resource
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.Import
import Kernel.Common.LocalArchive qualified as LocalArchive
import Kernel.Common.Module qualified as M
import Kernel.Common.Platform qualified as P
import Kernel.Common.Target qualified as Target
import Kernel.Common.ZenConfig qualified as Z
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Trope qualified as Trope
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Language.Common.ModuleID qualified as MID
import Language.Term.Trace qualified as TermTrace
import Logger.Handle qualified as Logger
import Path

data Handle = Handle
  { artifactHandle :: Artifact.Handle,
    antecedentHandle :: Antecedent.Handle,
    consoleHandle :: Console.Handle,
    platformHandle :: Platform.Handle,
    dataHandle :: Data.Handle,
    defHandle :: Definition.Handle,
    tropeHandle :: Trope.Handle,
    envHandle :: Env.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    importedTypeDefCacheHandle :: ImportedTypeDefCache.Handle,
    keyArgHandle :: KeyArg.Handle,
    moduleHandle :: Module.Handle,
    modulePathHandle :: ModulePath.Handle,
    optDataHandle :: OptimizableData.Handle,
    pathHandle :: Path.Handle,
    resourceHandle :: Resource.Handle,
    exposeHandle :: Expose.Handle,
    loggerHandle :: Logger.Handle,
    typeHandle :: Type.Handle,
    weakDefHandle :: WeakDef.Handle,
    weakTypeDefHandle :: WeakTypeDef.Handle,
    typeDefHandle :: TypeDef.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    unusedTopLevelNameHandle :: UnusedTopLevelName.Handle,
    termTraceHandle :: TermTrace.Handle,
    publicModuleReachabilityRef :: IORef (Map.HashMap MID.ModuleID (S.Set MID.ModuleID)),
    presetCacheRef :: IORef (Map.HashMap MID.ModuleID [ImportItem]),
    isShiftMapRegisteredRef :: IORef Bool,
    localArchiveMap :: LocalArchive.LocalArchiveMap
  }

new :: Console.Handle -> Logger.Handle -> LocalArchive.LocalArchiveMap -> Maybe (Path Abs File) -> Maybe M.TargetName -> App Handle
new consoleHandle loggerHandle localArchiveMap moduleFilePathOrNone targetNameOrNone = do
  handleOrError <- liftIO $ newOrError consoleHandle loggerHandle localArchiveMap moduleFilePathOrNone targetNameOrNone
  liftEither handleOrError

newOrError :: Console.Handle -> Logger.Handle -> LocalArchive.LocalArchiveMap -> Maybe (Path Abs File) -> Maybe M.TargetName -> IO (Either E.Error Handle)
newOrError consoleHandle loggerHandle localArchiveMap moduleFilePathOrNone targetNameOrNone = do
  envHandleOrError <- Env.new moduleFilePathOrNone
  runApp $ do
    envHandle <- liftEither envHandleOrError
    let mainModule = Env.getMainModule envHandle
    selector <- liftEither $ resolvePlatformSelector mainModule targetNameOrNone
    platformHandle <- Platform.new loggerHandle selector
    liftIO $ newHandle consoleHandle loggerHandle envHandle platformHandle localArchiveMap

newHandle :: Console.Handle -> Logger.Handle -> Env.Handle -> Platform.Handle -> LocalArchive.LocalArchiveMap -> IO Handle
newHandle consoleHandle loggerHandle envHandle platformHandle localArchiveMap = do
  let mainModule = Env.getMainModule envHandle
  Logger.setModuleDir loggerHandle mainModule
  optDataHandle <- OptimizableData.new
  resourceHandle <- Resource.new
  exposeHandle <- Expose.new
  typeHandle <- Type.new
  dataHandle <- Data.new
  pathHandle <- Path.new mainModule platformHandle loggerHandle
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  antecedentHandle <- Antecedent.new
  modulePathHandle <- ModulePath.new moduleHandle antecedentHandle mainModule
  keyArgHandle <- KeyArg.new mainModule modulePathHandle
  weakDefHandle <- WeakDef.new
  weakTypeDefHandle <- WeakTypeDef.new
  defHandle <- Definition.new
  tropeHandle <- Trope.new
  typeDefHandle <- TypeDef.new
  importedTypeDefCacheHandle <- ImportedTypeDefCache.new
  globalNameMapHandle <- GlobalNameMap.new
  unusedTopLevelNameHandle <- UnusedTopLevelName.new mainModule
  termTraceHandle <- TermTrace.new
  publicModuleReachabilityRef <- newIORef Map.empty
  presetCacheRef <- newIORef Map.empty
  isShiftMapRegisteredRef <- newIORef False
  return $ Handle {..}

resolvePlatformSelector :: M.MainModule -> Maybe M.TargetName -> Either E.Error P.PlatformSelector
resolvePlatformSelector mainModule targetNameOrNone = do
  case targetNameOrNone of
    Nothing ->
      Right $ Z.platform $ M.moduleZenConfig $ M.extractModule mainModule
    Just targetName ->
      case Map.lookup targetName (M.moduleTarget (M.extractModule mainModule)) of
        Just summary ->
          Right $ Target.platform summary
        Nothing ->
          Left $ E.newError' $ "No such target exists: " <> targetName
