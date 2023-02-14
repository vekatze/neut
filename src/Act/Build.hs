module Act.Build
  ( build,
    Context,
  )
where

import Context.Cache qualified as Cache
import Context.External qualified as External
import Context.LLVM qualified as LLVM
import Control.Monad
import Data.Foldable
import Entity.Config.Build
import Scene.Clarify qualified as Clarify
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.Execute qualified as Execute
import Scene.Initialize qualified as Initialize
import Scene.Link qualified as Link
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import Prelude hiding (log)

class
  ( Cache.Context m,
    Clarify.Context m,
    Collect.Context m,
    Elaborate.Context m,
    Emit.Context m,
    Initialize.Context m,
    LLVM.Context m,
    Link.Context m,
    External.Context m,
    Execute.Context m,
    Lower.Context m,
    Parse.Context m,
    Unravel.Context m
  ) =>
  Context m

build :: Context m => Config -> m ()
build cfg = do
  Initialize.initializeCompiler (logCfg cfg) True (mClangOptString cfg)
  targetList <- Collect.collectTargetList $ mTarget cfg
  forM_ targetList $ \target -> do
    Initialize.initializeForTarget
    (_, _, isObjectAvailable, dependenceSeq) <- Unravel.unravel target
    forM_ dependenceSeq $ \source -> do
      Initialize.initializeForSource source
      virtualCode <- Parse.parse >>= Elaborate.elaborate >>= Clarify.clarify
      Cache.whenCompilationNecessary (outputKindList cfg) source $ do
        Lower.lower virtualCode >>= Emit.emit >>= LLVM.emit (outputKindList cfg)
    Link.link target (shouldSkipLink cfg) isObjectAvailable (toList dependenceSeq)
    when (shouldExecute cfg) $
      Execute.execute target
