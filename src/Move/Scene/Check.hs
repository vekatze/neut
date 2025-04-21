module Move.Scene.Check
  ( check,
    checkModule,
    checkSingle,
    checkAll,
  )
where

import Move.Context.App
import Move.Context.Debug (report)
import Move.Context.Env (getMainModule)
import Move.Context.Throw qualified as Throw
import Control.Monad
import Data.Text qualified as T
import Rule.Module qualified as M
import Rule.Remark
import Rule.Source (Source (sourceFilePath))
import Rule.Target
import Path
import Move.Scene.Elaborate qualified as Elaborate
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Load qualified as Load
import Move.Scene.Module.Reflect (getAllDependencies)
import Move.Scene.Parse qualified as Parse
import Move.Scene.Unravel qualified as Unravel

check :: App [Remark]
check = do
  getMainModule >>= _check Peripheral

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
      report $ "Checking: " <> T.pack (toFilePath $ sourceFilePath source)
      void $ Parse.parse target source cacheOrContent >>= Elaborate.elaborate target

checkAll :: App [Remark]
checkAll = do
  mainModule <- getMainModule
  deps <- getAllDependencies mainModule
  forM_ deps $ \(_, m) -> checkModule m
  checkModule mainModule
