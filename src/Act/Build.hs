module Act.Build (build) where

import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Data.Foldable
import Entity.Config.Build
import Scene.Clarify qualified as Clarify
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.Execute qualified as Execute
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Scene.Install qualified as Install
import Scene.Link qualified as Link
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import Prelude hiding (log)

build :: Config -> App ()
build cfg = do
  LLVM.ensureSetupSanity cfg
  Initialize.initializeCompiler (remarkCfg cfg) (mClangOptString cfg)
  Env.setBuildMode $ buildMode cfg
  Module.getMainModule >>= Fetch.fetch
  mDir <- mapM Path.getInstallDir (installDir cfg)
  targetList <- Collect.collectTargetList $ mTarget cfg
  forM_ targetList $ \target -> do
    Initialize.initializeForTarget
    (artifactTime, dependenceSeq) <- Unravel.unravel target
    forM_ dependenceSeq $ \source -> do
      Initialize.initializeForSource source
      virtualCode <- Parse.parse >>= Elaborate.elaborate >>= Clarify.clarify
      Cache.whenCompilationNecessary (outputKindList cfg) source $ do
        Lower.lower virtualCode >>= Emit.emit >>= LLVM.emit (outputKindList cfg)
    Link.link target (shouldSkipLink cfg) artifactTime (toList dependenceSeq)
    when (shouldExecute cfg) $
      Execute.execute target (args cfg)
    mapM_ (Install.install target) mDir
