module Act.Build (build) where

import Context.App
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Module (getMainModule)
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Entity.Config.Build
import Entity.Target
import Scene.Build qualified as Build
import Scene.Collect qualified as Collect
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

build :: Config -> App ()
build cfg = do
  setup cfg
  target <- Collect.getMainTarget $ targetName cfg
  mainModule <- getMainModule
  Build.buildTarget (fromConfig cfg) mainModule (Main target)

setup :: Config -> App ()
setup cfg = do
  LLVM.ensureSetupSanity cfg
  Path.ensureNotInLibDir
  Initialize.initializeCompiler (remarkCfg cfg)
  Env.setBuildMode $ buildMode cfg
  Module.getMainModule >>= Fetch.fetch

fromConfig :: Config -> Build.Axis
fromConfig cfg =
  Build.Axis
    { _outputKindList = outputKindList cfg,
      _shouldSkipLink = shouldSkipLink cfg,
      _shouldExecute = shouldExecute cfg,
      _installDir = installDir cfg,
      _executeArgs = args cfg
    }
