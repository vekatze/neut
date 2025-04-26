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
import Move.Context.Throw qualified as Throw
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
    loadHandle :: Load.Handle,
    unravelHandle :: Unravel.Handle,
    parseHandle :: Parse.Handle,
    envHandle :: Env.Handle
  }

new :: App Handle
new = do
  debugHandle <- Debug.new
  loadHandle <- Load.new
  unravelHandle <- Unravel.new
  parseHandle <- Parse.new
  envHandle <- Env.new
  return $ Handle {..}

check :: App [Remark]
check = do
  h <- new
  M.MainModule mainModule <- liftIO $ Env.getMainModule (envHandle h)
  _check h Peripheral mainModule

checkModule :: M.Module -> App [Remark]
checkModule baseModule = do
  h <- new
  _check h Peripheral baseModule

checkAll :: App [Remark]
checkAll = do
  he <- Env.new
  mainModule <- liftIO $ Env.getMainModule he
  hm <- Module.new
  deps <- toApp $ Module.getAllDependencies hm mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> checkModule m
  checkModule (extractModule mainModule)

checkSingle :: Elaborate.HandleEnv -> M.Module -> Path Abs File -> App [Remark]
checkSingle hRootEnv baseModule path = do
  h <- new
  _check' h hRootEnv (PeripheralSingle path) baseModule

_check :: Handle -> Target -> M.Module -> App [Remark]
_check h target baseModule = do
  Throw.collectLogs $ do
    InitTarget.new >>= liftIO . InitTarget.initializeForTarget
    (_, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) baseModule target
    contentSeq <- toApp $ Load.load (loadHandle h) target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      hEnv <- liftIO Elaborate.createNewEnv
      checkSource h hEnv target source cacheOrContent

_check' :: Handle -> Elaborate.HandleEnv -> Target -> M.Module -> App [Remark]
_check' h hRootEnv target baseModule = do
  Throw.collectLogs $ do
    InitTarget.new >>= liftIO . InitTarget.initializeForTarget
    (_, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) baseModule target
    contentSeq <- toApp $ Load.load (loadHandle h) target dependenceSeq
    case unsnoc contentSeq of
      Nothing ->
        return ()
      Just (deps, (rootSource, rootCacheOrContent)) -> do
        forM_ deps $ \(source, cacheOrContent) -> do
          hEnv <- liftIO Elaborate.createNewEnv
          checkSource h hEnv target source cacheOrContent
        checkSource h hRootEnv target rootSource rootCacheOrContent

checkSource :: Handle -> Elaborate.HandleEnv -> Target -> Source -> Either Cache T.Text -> App ()
checkSource h hEnv target source cacheOrContent = do
  InitSource.new >>= \hInit -> toApp (InitSource.initializeForSource hInit source)
  toApp $ Debug.report (debugHandle h) $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hElaborate <- Elaborate.new hEnv
  void $
    toApp $
      Parse.parse (parseHandle h) target source cacheOrContent
        >>= Elaborate.elaborate hElaborate target

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
