module Move.Scene.Check
  ( check,
    checkModule,
    checkSingle,
    checkAll,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Debug (report)
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.Throw qualified as Throw
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetModule qualified as Module
import Move.Scene.Parse qualified as Parse
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Module (extractModule)
import Rule.Module qualified as M
import Rule.Remark
import Rule.Source (Source (sourceFilePath))
import Rule.Target

check :: App [Remark]
check = do
  M.MainModule mainModule <- getMainModule
  _check Peripheral mainModule

checkSingle :: M.Module -> Path Abs File -> App [Remark]
checkSingle baseModule path = do
  _check (PeripheralSingle path) baseModule

checkModule :: M.Module -> App [Remark]
checkModule baseModule = do
  _check Peripheral baseModule

_check :: Target -> M.Module -> App [Remark]
_check target baseModule = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    (_, dependenceSeq) <- Unravel.unravel baseModule target
    contentSeq <- Load.load target dependenceSeq
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      Initialize.initializeForSource source
      toApp $ report $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
      void $ Parse.parse target source cacheOrContent >>= Elaborate.elaborate target

checkAll :: App [Remark]
checkAll = do
  mainModule <- getMainModule
  counter <- asks App.counter
  mcm <- asks App.moduleCacheMap
  let h = Module.Handle {counter, mcm}
  deps <- toApp $ Module.getAllDependencies h mainModule (extractModule mainModule)
  forM_ deps $ \(_, m) -> checkModule m
  checkModule (extractModule mainModule)
