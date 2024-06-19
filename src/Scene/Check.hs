module Scene.Check
  ( check,
    checkModule,
    checkSingle,
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
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import UnliftIO.Async

check :: App [Remark]
check = do
  getMainModule >>= _check Peripheral

checkSingle :: Path Abs File -> App [Remark]
checkSingle path = do
  getMainModule >>= _check (PeripheralSingle path)

checkModule :: M.Module -> App [Remark]
checkModule baseModule = do
  _check Peripheral baseModule

_check :: Target -> M.Module -> App [Remark]
_check target baseModule = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    (_, dependenceSeq) <- Unravel.unravel baseModule target
    contentSeq <- forConcurrently dependenceSeq $ \source -> do
      cacheOrContent <- Load.load source
      return (source, cacheOrContent)
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      Initialize.initializeForSource source
      void $ Parse.parse source cacheOrContent >>= Elaborate.elaborate
