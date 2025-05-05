module Main.Move.Scene.Check
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
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Log
import Logger.Rule.Log qualified as L
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.GlobalRemark qualified as GlobalRemark
import Main.Move.Scene.Elaborate qualified as Elaborate
import Main.Move.Scene.Elaborate.Handle.Elaborate qualified as Elaborate
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Init.Local qualified as Local
import Main.Move.Scene.Load qualified as Load
import Main.Move.Scene.Module.GetModule qualified as GetModule
import Main.Move.Scene.Parse qualified as Parse
import Main.Move.Scene.Unravel qualified as Unravel
import Main.Rule.Cache
import Main.Rule.Module (extractModule)
import Main.Rule.Module qualified as M
import Main.Rule.Source (Source (sourceFilePath))
import Main.Rule.Target
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
  let parseHandle = Parse.new (baseHandle h) localHandle
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
