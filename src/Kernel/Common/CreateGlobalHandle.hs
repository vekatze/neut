module Kernel.Common.CreateGlobalHandle
  ( Handle (..),
    new,
    newOrError,
  )
where

import App.Error qualified as E
import App.Run (run)
import CommandParser.Config.Remark qualified as Remark
import Console.CreateHandle qualified as Console
import Console.Handle qualified as Console
import Control.Monad.Except (MonadError (throwError))
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
import Logger.CreateHandle qualified as Logger
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
    presetCacheRef :: IORef (Map.HashMap MID.ModuleID [ImportItem])
  }

new :: Remark.Config -> Maybe (Path Abs File) -> Maybe M.TargetName -> IO Handle
new cfg moduleFilePathOrNone targetNameOrNone = do
  handleOrError <- newOrError cfg moduleFilePathOrNone targetNameOrNone
  case handleOrError of
    Left (loggerHandle, err) -> do
      run loggerHandle $ throwError err
    Right v ->
      return v

newOrError :: Remark.Config -> Maybe (Path Abs File) -> Maybe M.TargetName -> IO (Either (Logger.Handle, E.Error) Handle)
newOrError cfg moduleFilePathOrNone targetNameOrNone = do
  consoleHandle <- Console.createHandle (Remark.shouldColorize cfg) (Remark.shouldColorize cfg) (Remark.reportMode cfg)
  loggerHandle <- Logger.createHandle consoleHandle
  envHandleOrError <- Env.new moduleFilePathOrNone
  case envHandleOrError of
    Left errors ->
      return $ Left (loggerHandle, errors)
    Right envHandle -> do
      let mainModule = Env.getMainModule envHandle
      case resolvePlatformSelector mainModule targetNameOrNone of
        Left err ->
          return $ Left (loggerHandle, err)
        Right selector ->
          Right <$> newHandle consoleHandle loggerHandle envHandle selector

newHandle :: Console.Handle -> Logger.Handle -> Env.Handle -> P.PlatformSelector -> IO Handle
newHandle consoleHandle loggerHandle envHandle selector = do
  let mainModule = Env.getMainModule envHandle
  platformHandle <- Platform.new loggerHandle selector
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
