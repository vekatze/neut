module Move.Scene.Check
  ( check,
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
import Move.Context.Env (getMainModule)
import Move.Context.Throw qualified as Throw
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Initialize qualified as Initialize
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

check :: App [Remark]
check = do
  M.MainModule mainModule <- getMainModule
  _check Peripheral mainModule

checkModule :: M.Module -> App [Remark]
checkModule baseModule = do
  _check Peripheral baseModule

checkAll :: App [Remark]
checkAll = do
  mainModule <- getMainModule
  h <- Module.new
  deps <- toApp $ Module.getAllDependencies h mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> checkModule m
  checkModule (extractModule mainModule)

checkSingle :: Elaborate.HandleEnv -> M.Module -> Path Abs File -> App [Remark]
checkSingle h baseModule path = do
  _check' h (PeripheralSingle path) baseModule

_check :: Target -> M.Module -> App [Remark]
_check target baseModule = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    h <- Unravel.new
    (_, dependenceSeq) <- toApp $ Unravel.unravel h baseModule target
    h' <- Load.new
    contentSeq <- toApp $ Load.load h' target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      hEnv <- liftIO Elaborate.createNewEnv
      checkSource hEnv target source cacheOrContent

_check' :: Elaborate.HandleEnv -> Target -> M.Module -> App [Remark]
_check' hRootEnv target baseModule = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    h <- Unravel.new
    (_, dependenceSeq) <- toApp $ Unravel.unravel h baseModule target
    h' <- Load.new
    contentSeq <- toApp $ Load.load h' target dependenceSeq
    case unsnoc contentSeq of
      Nothing ->
        return ()
      Just (deps, (rootSource, rootCacheOrContent)) -> do
        forM_ deps $ \(source, cacheOrContent) -> do
          hEnv <- liftIO Elaborate.createNewEnv
          checkSource hEnv target source cacheOrContent
        checkSource hRootEnv target rootSource rootCacheOrContent

checkSource :: Elaborate.HandleEnv -> Target -> Source -> Either Cache T.Text -> App ()
checkSource hEnv target source cacheOrContent = do
  Initialize.initializeForSource source
  h'' <- Debug.new
  toApp $ Debug.report h'' $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
  hParse <- Parse.new
  hElaborate <- Elaborate.new hEnv
  void $ toApp $ Parse.parse hParse target source cacheOrContent >>= Elaborate.elaborate hElaborate target

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
