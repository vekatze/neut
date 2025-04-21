module Act.Build (build) where

import Context.App
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Control.Monad
import Rule.Config.Build
import Rule.Target
import Scene.Build qualified as Build
import Scene.Collect qualified as Collect
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

build :: Config -> App ()
build cfg = do
  setup cfg
  target <- Collect.getMainTarget $ targetName cfg
  mainModule <- Env.getMainModule
  Build.buildTarget (fromConfig cfg) mainModule (Main target)

setup :: Config -> App ()
setup cfg = do
  LLVM.ensureSetupSanity cfg
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInDependencyDir
  Env.setBuildMode $ buildMode cfg
  Env.getMainModule >>= Fetch.fetch

fromConfig :: Config -> Build.Axis
fromConfig cfg =
  Build.Axis
    { _outputKindList = outputKindList cfg,
      _shouldSkipLink = shouldSkipLink cfg,
      _shouldExecute = shouldExecute cfg,
      _installDir = installDir cfg,
      _executeArgs = args cfg
    }
