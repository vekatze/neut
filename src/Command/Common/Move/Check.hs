module Command.Common.Move.Check
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
import Error.Move.Run (runEIO)
import Error.Rule.EIO (EIO)
import Error.Rule.Error qualified as E
import Kernel.Elaborate.Move.Elaborate qualified as Elaborate
import Kernel.Elaborate.Move.Internal.Handle.Elaborate qualified as Elaborate
import Kernel.Load.Move.Load qualified as Load
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.GlobalRemark qualified as GlobalRemark
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Move.Scene.Init.Local qualified as Local
import Kernel.Move.Scene.Module.GetModule qualified as GetModule
import Kernel.Parse.Move.Parse qualified as Parse
import Kernel.Common.Rule.Cache
import Kernel.Common.Rule.Module (extractModule)
import Kernel.Common.Rule.Module qualified as M
import Kernel.Common.Rule.Source (Source (sourceFilePath))
import Kernel.Common.Rule.Target
import Kernel.Unravel.Move.Unravel qualified as Unravel
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Log
import Logger.Rule.Log qualified as L
import Path

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new ::
  Base.Handle ->
  Handle
new baseHandle = do
  Handle {..}

check :: Handle -> EIO [Log]
check h = do
  let M.MainModule mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  liftIO $ _check h Peripheral mainModule

checkModule :: Handle -> M.Module -> IO [Log]
checkModule h baseModule = do
  _check h Peripheral baseModule

checkAll :: Handle -> EIO [Log]
checkAll h = do
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  let getModuleHandle = GetModule.new (baseHandle h)
  deps <- GetModule.getAllDependencies getModuleHandle mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> liftIO $ checkModule h m
  liftIO $ checkModule h (extractModule mainModule)

checkSingle :: Handle -> M.Module -> Path Abs File -> EIO (Maybe Elaborate.Handle)
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> IO [Log]
_check h target baseModule = do
  collectLogs (Base.globalRemarkHandle (baseHandle h)) $ do
    let loadHandle = Load.new (baseHandle h)
    unravelHandle <- liftIO $ Unravel.new (baseHandle h)
    (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
    contentSeq <- Load.load loadHandle target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      checkSource h target source cacheOrContent

_check' :: Handle -> Target -> M.Module -> EIO (Maybe Elaborate.Handle)
_check' h target baseModule = do
  unravelHandle <- liftIO $ Unravel.new (baseHandle h)
  let loadHandle = Load.new (baseHandle h)
  (_, dependenceSeq) <- Unravel.unravel unravelHandle baseModule target
  contentSeq <- Load.load loadHandle target dependenceSeq
  case unsnoc contentSeq of
    Nothing ->
      return Nothing
    Just (deps, (rootSource, rootCacheOrContent)) -> do
      forM_ deps $ \(source, cacheOrContent) -> do
        checkSource h target source cacheOrContent
      Just <$> checkSource h target rootSource rootCacheOrContent

checkSource :: Handle -> Target -> Source -> Either Cache T.Text -> EIO Elaborate.Handle
checkSource h target source cacheOrContent = do
  localHandle <- Local.new (baseHandle h) source
  parseHandle <- liftIO $ Parse.new (baseHandle h) localHandle
  elaborateHandle <- liftIO $ Elaborate.new (baseHandle h) localHandle source
  liftIO $
    Logger.report (Base.loggerHandle (baseHandle h)) $
      "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  (cacheOrStmtList, logs) <- Parse.parse parseHandle target source cacheOrContent
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
