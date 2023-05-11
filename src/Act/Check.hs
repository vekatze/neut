module Act.Check (check) where

import Context.App
import Context.Throw qualified as Throw
import Control.Monad
import Entity.Config.Check
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Initialize qualified as Initialize
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel

check :: Config -> App ()
check cfg = do
  let runner = if shouldInsertPadding cfg then id else Throw.run'
  runner $ do
    Initialize.initializeCompiler (remarkCfg cfg) Nothing
    sgls <- Collect.collectSourceList (mFilePathString cfg)
    forM_ sgls $ \sgl -> do
      (_, dependenceSeq) <- Unravel.unravelFromSGL sgl
      forM_ dependenceSeq $ \source -> do
        Initialize.initializeForSource source
        void $ Parse.parse >>= Elaborate.elaborate
