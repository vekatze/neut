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
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetModule qualified as Module
import Move.Scene.Parse qualified as Parse
import Move.Scene.Unravel qualified as Unravel
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
    locatorHandle :: Locator.Handle,
    tagHandle :: Tag.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> Tag.Handle -> App Handle
new envHandle gensymHandle locatorHandle tagHandle = do
  debugHandle <- Debug.new
  loadHandle <- Load.new envHandle
  unravelHandle <- Unravel.new envHandle gensymHandle locatorHandle tagHandle
  parseHandle <- Parse.new envHandle gensymHandle locatorHandle tagHandle
  moduleHandle <- Module.new gensymHandle
  initSourceHandle <- InitSource.new envHandle locatorHandle tagHandle
  initTargetHandle <- InitTarget.new envHandle gensymHandle locatorHandle tagHandle
  return $ Handle {..}

check :: Handle -> App [Remark]
check h = do
  M.MainModule mainModule <- toApp $ Env.getMainModule (envHandle h)
  _check h Peripheral mainModule

checkModule :: Handle -> M.Module -> App [Remark]
checkModule h baseModule = do
  _check h Peripheral baseModule

checkAll :: Handle -> App [Remark]
checkAll h = do
  mainModule <- toApp $ Env.getMainModule (envHandle h)
  deps <- toApp $ Module.getAllDependencies (moduleHandle h) mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> checkModule h m
  checkModule h (extractModule mainModule)

checkSingle :: Handle -> M.Module -> Path Abs File -> App Elaborate.HandleEnv
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> App [Remark]
_check h target baseModule = do
  Throw.collectLogs $ do
    liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
    (_, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) baseModule target
    contentSeq <- toApp $ Load.load (loadHandle h) target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      checkSource h target source cacheOrContent

_check' :: Handle -> Target -> M.Module -> App Elaborate.HandleEnv
_check' h target baseModule = do
  liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
  (_, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) baseModule target
  contentSeq <- toApp $ Load.load (loadHandle h) target dependenceSeq
  case unsnoc contentSeq of
    Nothing ->
      liftIO Elaborate.createNewEnv
    Just (deps, (rootSource, rootCacheOrContent)) -> do
      forM_ deps $ \(source, cacheOrContent) -> do
        checkSource h target source cacheOrContent
      checkSource' h target rootSource rootCacheOrContent

checkSource :: Handle -> Target -> Source -> Either Cache T.Text -> App ()
checkSource h target source cacheOrContent = do
  toApp (InitSource.initializeForSource (initSourceHandle h) source)
  toApp $ Debug.report (debugHandle h) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hElaborate <- Elaborate.new (envHandle h) (gensymHandle h) (locatorHandle h) (tagHandle h)
  void $
    toApp $
      Parse.parse (parseHandle h) target source cacheOrContent
        >>= Elaborate.elaborate hElaborate target

checkSource' :: Handle -> Target -> Source -> Either Cache T.Text -> App Elaborate.HandleEnv
checkSource' h target source cacheOrContent = do
  toApp (InitSource.initializeForSource (initSourceHandle h) source)
  toApp $ Debug.report (debugHandle h) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hElaborate <- Elaborate.new (envHandle h) (gensymHandle h) (locatorHandle h) (tagHandle h)
  toApp $
    Parse.parse (parseHandle h) target source cacheOrContent
      >>= Elaborate.elaborateThenInspect hElaborate target

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
