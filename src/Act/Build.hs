module Act.Build
  ( build,
    Context,
  )
where

import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Control.Monad
import Data.Foldable
import Data.Maybe
import Entity.Config.Build
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Scene.Clarify qualified as Clarify
import Scene.Collect qualified as Collect
import Scene.Elaborate qualified as Elaborate
import Scene.Emit qualified as Emit
import Scene.Initialize qualified as Initialize
import Scene.Link qualified as Link
import Scene.Lower qualified as Lower
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel
import Prelude hiding (log)

class
  ( Clarify.Context m,
    Collect.Context m,
    Elaborate.Context m,
    Emit.Context m,
    Env.Context m,
    Initialize.Context m,
    LLVM.Context m,
    Link.Context m,
    Lower.Context m,
    Parse.Context m,
    Unravel.Context m
  ) =>
  Context m

build :: Context m => Config -> m ()
build cfg = do
  Initialize.initializeCompiler (logCfg cfg) True
  targetList <- Collect.collectTargetList $ mTarget cfg
  forM_ targetList $ \target -> do
    Initialize.initializeForTarget
    (_, _, isObjectAvailable, dependenceSeq) <- Unravel.unravel target
    mapM_ (compile (outputKindList cfg)) dependenceSeq
    Link.link target (fromMaybe "" (mClangOptString cfg)) (shouldSkipLink cfg) isObjectAvailable (toList dependenceSeq)

compile ::
  Context m =>
  [OK.OutputKind] ->
  Source.Source ->
  m ()
compile outputKindList source = do
  Initialize.initializeForSource source
  hasLLVMSet <- Env.getHasLLVMSet
  hasObjectSet <- Env.getHasObjectSet
  virtualCode <- Parse.parse >>= Elaborate.elaborate >>= Clarify.clarify
  if Source.isCompilationSkippable hasLLVMSet hasObjectSet outputKindList source
    then return ()
    else Lower.lower virtualCode >>= Emit.emit >>= LLVM.emit outputKindList
