module Command.Common.Check
  ( Handle,
    new,
    check,
    checkOrFail,
    checkSingle,
    checkAll,
    checkAllOrFail,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (forP, raiseError', runApp)
import Command.Common.Dependency qualified as Dependency
import Console.Handle qualified as Console
import Control.Concurrent (getNumCapabilities)
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module (extractModule)
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source (Source (sourceFilePath))
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Kernel.Common.Trace qualified as Trace
import Kernel.Elaborate.Elaborate qualified as Elaborate
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Load.Load qualified as Load
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Kernel.Parse.Interpret qualified as Interpret
import Kernel.Parse.Parse qualified as Parse
import Kernel.Unravel.Unravel qualified as Unravel
import Language.WeakTerm.WeakStmt (WeakStmt)
import Logger.Debug qualified as Logger
import Logger.Log
import Logger.Log qualified as L
import Path

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  Handle {..}

check :: Handle -> App [Log]
check h = do
  let M.MainModule mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  liftIO $ _check h Peripheral mainModule

checkOrFail :: Handle -> App [Log]
checkOrFail h = do
  logs <- check h
  throwIfFailure logs
  return logs

checkModule :: Handle -> M.Module -> IO [Log]
checkModule h = do
  _check h Peripheral

checkAll :: Handle -> App [Log]
checkAll h = do
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  let getModuleHandle = GetModule.new $ Global.moduleHandle $ globalHandle h
  deps <- GetModule.getAllDependencies getModuleHandle mainModule (extractModule mainModule)
  depLogs <- fmap concat $ forM deps $ \(_, m) -> liftIO $ checkModule h m
  mainLogs <- liftIO $ checkModule h (extractModule mainModule)
  return $ depLogs <> mainLogs

checkAllOrFail :: Handle -> App [Log]
checkAllOrFail h = do
  logs <- checkAll h
  throwIfFailure logs
  return logs

throwIfFailure :: [Log] -> App ()
throwIfFailure logs = do
  when (any L.isFailure logs) $
    throwError $
      E.MakeError logs

checkSingle :: Handle -> M.Module -> Path Abs File -> App (Maybe Elaborate.Handle)
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> IO [Log]
_check h target baseModule = do
  collectLogs (Global.globalRemarkHandle (globalHandle h)) $ do
    let loadHandle = Load.new (globalHandle h)
    unravelHandle <- liftIO $ Unravel.new (globalHandle h)
    (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
    sourceDependencyMap <- liftIO $ Unravel.getSourceDependencyMap unravelHandle dependenceSeq
    traceConfig <- newTraceConfig h
    contentSeq <- Load.load loadHandle (Trace.isEnabled traceConfig) target dependenceSeq
    cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
    cacheOrStmtList <- forP cacheOrProgList $ \(gensymHandle, localHandle, (source, cacheOrProg)) -> do
      liftIO $
        Logger.report (Global.loggerHandle (globalHandle h)) $
          "Interpreting: " <> T.pack (toFilePath $ Source.sourceFilePath source)
      interpretHandle <- liftIO $ Interpret.new gensymHandle (globalHandle h) localHandle (Source.sourceModule source)
      item <- Interpret.interpret interpretHandle target source cacheOrProg
      return (gensymHandle, localHandle, (source, item))
    numCapabilities <- liftIO getNumCapabilities
    void $ Dependency.run numCapabilities sourceDependencyMap Parse.getSourcePath cacheOrStmtList $ \(gensymHandle, localHandle, (source, cacheOrContent)) -> do
      checkSource h gensymHandle traceConfig localHandle target source cacheOrContent
    registerUnusedTopLevelNameRemarks h

_check' :: Handle -> Target -> M.Module -> App (Maybe Elaborate.Handle)
_check' h target baseModule = do
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  let loadHandle = Load.new (globalHandle h)
  (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
  sourceDependencyMap <- liftIO $ Unravel.getSourceDependencyMap unravelHandle dependenceSeq
  traceConfig <- newTraceConfig h
  contentSeq <- Load.load loadHandle (Trace.isEnabled traceConfig) target dependenceSeq
  cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
  cacheOrStmtList <- forP cacheOrProgList $ \(gensymHandle, localHandle, (source, cacheOrProg)) -> do
    liftIO $
      Logger.report (Global.loggerHandle (globalHandle h)) $
        "Interpreting: " <> T.pack (toFilePath $ Source.sourceFilePath source)
    interpretHandle <- liftIO $ Interpret.new gensymHandle (globalHandle h) localHandle (Source.sourceModule source)
    item <- Interpret.interpret interpretHandle target source cacheOrProg
    return (gensymHandle, localHandle, (source, item))
  case unsnoc cacheOrStmtList of
    Nothing -> do
      registerUnusedTopLevelNameRemarks h
      return Nothing
    Just (deps, (rootGensymHandle, rootLocalHandle, (rootSource, rootCacheOrContent))) -> do
      numCapabilities <- liftIO getNumCapabilities
      void $ Dependency.run numCapabilities sourceDependencyMap Parse.getSourcePath deps $ \(gensymHandle, localHandle, (source, cacheOrContent)) -> do
        checkSource h gensymHandle traceConfig localHandle target source cacheOrContent
      result <- Just <$> checkSource h rootGensymHandle traceConfig rootLocalHandle target rootSource rootCacheOrContent
      registerUnusedTopLevelNameRemarks h
      return result

checkSource :: Handle -> Gensym.Handle -> Trace.Config -> Local.Handle -> Target -> Source -> (Either Cache [WeakStmt], [Log]) -> App Elaborate.Handle
checkSource h gensymHandle traceConfig localHandle target source (cacheOrStmtList, logs) = do
  elaborateHandle <- liftIO $ Elaborate.new gensymHandle (globalHandle h) traceConfig localHandle source
  liftIO $
    Logger.report (Global.loggerHandle (globalHandle h)) $
      "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  void $ Elaborate.elaborate elaborateHandle target logs cacheOrStmtList
  return elaborateHandle

newTraceConfig :: Handle -> App Trace.Config
newTraceConfig h = do
  let traceReport = Console.getTraceConfig $ Global.consoleHandle $ globalHandle h
  either raiseError' return $ Trace.new (Env.getMainModule $ Global.envHandle $ globalHandle h) traceReport

registerUnusedTopLevelNameRemarks :: Handle -> App ()
registerUnusedTopLevelNameRemarks h = do
  modulePathMap <- liftIO $ ModulePath.get $ Global.modulePathHandle $ globalHandle h
  logs <- liftIO $ UnusedTopLevelName.flushRemarks modulePathMap $ Global.unusedTopLevelNameHandle $ globalHandle h
  liftIO $ GlobalRemark.insert (Global.globalRemarkHandle $ globalHandle h) logs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

collectLogs :: GlobalRemark.Handle -> App () -> IO [L.Log]
collectLogs h c = do
  resultOrErr <- runApp c
  remarkList <- liftIO $ GlobalRemark.get h
  case resultOrErr of
    Left (E.MakeError logList) ->
      return $ logList ++ remarkList
    Right _ ->
      return remarkList
