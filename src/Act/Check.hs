module Act.Check
  ( check,
    Config (..),
    Context,
  )
where

import Control.Monad
import Entity.Config.Check
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Initialize qualified as Initialize
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel

class
  ( Unravel.Context m,
    Parse.Context m,
    Collect.Context m,
    Initialize.Context m,
    Elaborate.Context m
  ) =>
  Context m

check :: Context m => Config -> m ()
check cfg = do
  Initialize.initializeCompiler (logCfg cfg) True Nothing
  sgls <- Collect.collectSourceList (mFilePathString cfg)
  forM_ sgls $ \sgl -> do
    (_, _, _, dependenceSeq) <- Unravel.unravelFromSGL sgl
    forM_ dependenceSeq $ \source -> do
      Initialize.initializeForSource source
      void $ Parse.parse >>= Elaborate.elaborate
