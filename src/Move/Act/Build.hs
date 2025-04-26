module Move.Act.Build (build) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Scene.Build qualified as Build
import Move.Scene.Collect qualified as Collect
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Build
import Rule.Target
import Prelude hiding (log)

build :: Config -> App ()
build cfg = do
  setup cfg
  h <- Collect.new
  target <- toApp $ Collect.getMainTarget h $ targetName cfg
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  Build.buildTarget (fromConfig cfg) mainModule (Main target)

setup :: Config -> App ()
setup cfg = do
  toApp $ LLVM.ensureSetupSanity cfg
  hc <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  toApp $ Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode envHandle $ buildMode cfg
  h <- Fetch.new
  toApp $ Fetch.fetch h mainModule

fromConfig :: Config -> Build.Axis
fromConfig cfg =
  Build.Axis
    { _outputKindList = outputKindList cfg,
      _shouldSkipLink = shouldSkipLink cfg,
      _shouldExecute = shouldExecute cfg,
      _installDir = installDir cfg,
      _executeArgs = args cfg
    }
