module Move.Act.Build (build) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
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
  gensymHandle <- Gensym.new
  h <- Collect.new
  target <- toApp $ Collect.getMainTarget h $ targetName cfg
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  buildHandle <- Build.new (toBuildConfig cfg) gensymHandle
  Build.buildTarget buildHandle mainModule (Main target)

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

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = outputKindList cfg,
      shouldSkipLink = shouldSkipLink cfg,
      shouldExecute = shouldExecute cfg,
      installDir = installDir cfg,
      executeArgs = args cfg
    }
