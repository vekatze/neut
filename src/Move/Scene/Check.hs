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
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, collectLogs)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetModule qualified as Module
import Move.Scene.Parse qualified as Parse
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Path
import Rule.Cache
import Rule.Module (extractModule)
import Rule.Module qualified as M
import Rule.Remark
import Rule.Source (Source (sourceFilePath))
import Rule.Target

data Handle
  = Handle
  { debugHandle :: Debug.Handle,
    gensymHandle :: Gensym.Handle,
    loadHandle :: Load.Handle,
    unravelHandle :: Unravel.Handle,
    parseHandle :: Parse.Handle,
    moduleHandle :: Module.Handle,
    envHandle :: Env.Handle,
    initSourceHandle :: InitSource.Handle,
    initTargetHandle :: InitTarget.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    elaborateConfig :: Elaborate.Config
  }

new ::
  Debug.Handle ->
  Gensym.Handle ->
  Load.Handle ->
  Unravel.Handle ->
  Parse.Handle ->
  Module.Handle ->
  Env.Handle ->
  InitSource.Handle ->
  InitTarget.Handle ->
  GlobalRemark.Handle ->
  Elaborate.Config ->
  Handle
new debugHandle gensymHandle loadHandle unravelHandle parseHandle moduleHandle envHandle initSourceHandle initTargetHandle globalRemarkHandle elaborateConfig = do
  Handle {..}

check :: Handle -> EIO [Remark]
check h = do
  M.MainModule mainModule <- Env.getMainModule (envHandle h)
  liftIO $ _check h Peripheral mainModule

checkModule :: Handle -> M.Module -> IO [Remark]
checkModule h baseModule = do
  _check h Peripheral baseModule

checkAll :: Handle -> EIO [Remark]
checkAll h = do
  mainModule <- Env.getMainModule (envHandle h)
  deps <- Module.getAllDependencies (moduleHandle h) mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> liftIO $ checkModule h m
  liftIO $ checkModule h (extractModule mainModule)

checkSingle :: Handle -> M.Module -> Path Abs File -> EIO Elaborate.HandleEnv
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> IO [Remark]
_check h target baseModule = do
  collectLogs (globalRemarkHandle h) $ do
    liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
    (_, dependenceSeq) <- Unravel.unravel (unravelHandle h) baseModule target
    contentSeq <- Load.load (loadHandle h) target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      checkSource h target source cacheOrContent

_check' :: Handle -> Target -> M.Module -> EIO Elaborate.HandleEnv
_check' h target baseModule = do
  liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
  (_, dependenceSeq) <- Unravel.unravel (unravelHandle h) baseModule target
  contentSeq <- Load.load (loadHandle h) target dependenceSeq
  case unsnoc contentSeq of
    Nothing ->
      liftIO Elaborate.createNewEnv
    Just (deps, (rootSource, rootCacheOrContent)) -> do
      forM_ deps $ \(source, cacheOrContent) -> do
        checkSource h target source cacheOrContent
      checkSource' h target rootSource rootCacheOrContent

checkSource :: Handle -> Target -> Source -> Either Cache T.Text -> EIO ()
checkSource h target source cacheOrContent = do
  InitSource.initializeForSource (initSourceHandle h) source
  Debug.report (debugHandle h) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hElaborate <- liftIO $ Elaborate.new (elaborateConfig h) source
  void $
    Parse.parse (parseHandle h) target source cacheOrContent
      >>= Elaborate.elaborate hElaborate target

checkSource' :: Handle -> Target -> Source -> Either Cache T.Text -> EIO Elaborate.HandleEnv
checkSource' h target source cacheOrContent = do
  InitSource.initializeForSource (initSourceHandle h) source
  Debug.report (debugHandle h) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hElaborate <- liftIO $ Elaborate.new (elaborateConfig h) source
  Parse.parse (parseHandle h) target source cacheOrContent
    >>= Elaborate.elaborateThenInspect hElaborate target

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
