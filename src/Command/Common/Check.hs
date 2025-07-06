module Command.Common.Check
  ( Handle,
    new,
    check,
    checkModule,
    checkSingle,
    checkAll,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Error qualified as E
import Error.Run (forP, runEIO)
import Kernel.Common.Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Module (extractModule)
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source (Source (sourceFilePath))
import Kernel.Common.Target
import Kernel.Elaborate.Elaborate qualified as Elaborate
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Load.Load qualified as Load
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

check :: Handle -> EIO [Log]
check h = do
  let M.MainModule mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  liftIO $ _check h Peripheral mainModule

checkModule :: Handle -> M.Module -> IO [Log]
checkModule h baseModule = do
  _check h Peripheral baseModule

checkAll :: Handle -> EIO [Log]
checkAll h = do
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  let getModuleHandle = GetModule.new (globalHandle h)
  deps <- GetModule.getAllDependencies getModuleHandle mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> liftIO $ checkModule h m
  liftIO $ checkModule h (extractModule mainModule)

checkSingle :: Handle -> M.Module -> Path Abs File -> EIO (Maybe Elaborate.Handle)
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> IO [Log]
_check h target baseModule = do
  collectLogs (Global.globalRemarkHandle (globalHandle h)) $ do
    let loadHandle = Load.new (globalHandle h)
    unravelHandle <- liftIO $ Unravel.new (globalHandle h)
    (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
    contentSeq <- Load.load loadHandle target dependenceSeq
    cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
    cacheOrStmtList <- forP cacheOrProgList $ \(localHandle, (source, cacheOrProg)) -> do
      interpretHandle <- liftIO $ Interpret.new (globalHandle h) localHandle
      item <- Interpret.interpret interpretHandle target source cacheOrProg
      return (localHandle, (source, item))
    forM_ cacheOrStmtList $ \(localHandle, (source, cacheOrContent)) -> do
      checkSource h localHandle target source cacheOrContent

_check' :: Handle -> Target -> M.Module -> EIO (Maybe Elaborate.Handle)
_check' h target baseModule = do
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  let loadHandle = Load.new (globalHandle h)
  (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
  contentSeq <- Load.load loadHandle target dependenceSeq
  cacheOrProgList <- Parse.parse (globalHandle h) contentSeq
  cacheOrStmtList <- forP cacheOrProgList $ \(localHandle, (source, cacheOrProg)) -> do
    interpretHandle <- liftIO $ Interpret.new (globalHandle h) localHandle
    item <- Interpret.interpret interpretHandle target source cacheOrProg
    return (localHandle, (source, item))
  case unsnoc cacheOrStmtList of
    Nothing ->
      return Nothing
    Just (deps, (rootLocalHandle, (rootSource, rootCacheOrContent))) -> do
      forM_ deps $ \(localHandle, (source, cacheOrContent)) -> do
        checkSource h localHandle target source cacheOrContent
      Just <$> checkSource h rootLocalHandle target rootSource rootCacheOrContent

checkSource :: Handle -> Local.Handle -> Target -> Source -> (Either Cache [WeakStmt], [Log]) -> EIO Elaborate.Handle
checkSource h localHandle target source (cacheOrStmtList, logs) = do
  elaborateHandle <- liftIO $ Elaborate.new (globalHandle h) localHandle source
  liftIO $
    Logger.report (Global.loggerHandle (globalHandle h)) $
      "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  void $ Elaborate.elaborate elaborateHandle target logs cacheOrStmtList
  return elaborateHandle

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

collectLogs :: GlobalRemark.Handle -> EIO () -> IO [L.Log]
collectLogs h c = do
  resultOrErr <- runEIO c
  remarkList <- liftIO $ GlobalRemark.get h
  case resultOrErr of
    Left (E.MakeError logList) ->
      return $ logList ++ remarkList
    Right _ ->
      return remarkList
