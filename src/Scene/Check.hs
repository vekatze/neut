module Scene.Check
  ( check,
    checkSingle,
  )
where

import Context.App
import Context.Module (getMainModule)
import Context.Throw qualified as Throw
import Control.Monad
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
  _check Peripheral

checkSingle :: Path Abs File -> App [Remark]
checkSingle path = do
  _check (PeripheralSingle path)

_check :: Target -> App [Remark]
_check target = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    mainModule <- getMainModule
    (_, dependenceSeq) <- Unravel.unravel mainModule target
    contentSeq <- forConcurrently dependenceSeq $ \source -> do
      cacheOrContent <- Load.load target source
      return (source, cacheOrContent)
    forM_ contentSeq $ \(source, cacheOrContent) -> do
      Initialize.initializeForSource source
      void $ Parse.parse source cacheOrContent >>= Elaborate.elaborate target
