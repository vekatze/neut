module Scene.Check (check) where

import Context.App
import Context.Throw qualified as Throw
import Control.Monad
import Entity.Remark
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Initialize qualified as Initialize
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel

check :: Maybe FilePath -> App [Remark]
check mPath = do
  Throw.collectLogs $ do
    Initialize.initializeForTarget
    sgls <- Collect.collectSourceList mPath
    forM_ sgls $ \sgl -> do
      (_, dependenceSeq) <- Unravel.unravelFromSGL sgl
      forM_ dependenceSeq $ \source -> do
        Initialize.initializeForSource source
        void $ Parse.parse >>= Elaborate.elaborate
