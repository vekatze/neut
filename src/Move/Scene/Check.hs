module Move.Scene.Check
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
import Logger.Rule.Log
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, collectLogs)
import Move.Context.Env qualified as Env
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Elaborate.Handle.Elaborate qualified as Elaborate
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Local qualified as Local
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetModule qualified as GetModule
import Move.Scene.Parse qualified as Parse
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Cache
import Rule.Module (extractModule)
import Rule.Module qualified as M
import Rule.Source (Source (sourceFilePath))
import Rule.Target

newtype Handle
  = Handle
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
  liftIO $ Debug.report (Base.debugHandle (baseHandle h)) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  void $ Parse.parse parseHandle target source cacheOrContent >>= Elaborate.elaborate elaborateHandle target
  return elaborateHandle

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
