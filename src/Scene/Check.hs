module Scene.Check
  ( check,
    checkModule,
    checkSingle,
    checkAll,
  )
where

import Context.App
import Context.Env (getMainModule)
import Context.Throw qualified as Throw
import Control.Monad
import Entity.Module qualified as M
import Entity.Remark
import Entity.Target
import Path
import Scene.Elaborate qualified as Elaborate
import Scene.Initialize qualified as Initialize
import Scene.Load qualified as Load
import Scene.Module.Reflect (getAllDependencies)
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import UnliftIO.Async

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
    contentSeq <- pooledForConcurrently dependenceSeq $ \source -> do
      cacheOrContent <- Load.load target source
      return (source, cacheOrContent)
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      Initialize.initializeForSource source
      void $ Parse.parse target source cacheOrContent >>= Elaborate.elaborate target

checkAll :: App [Remark]
checkAll = do
  mainModule <- getMainModule
  deps <- getAllDependencies mainModule
  forM_ deps $ \(_, m) -> checkModule m
  checkModule mainModule
