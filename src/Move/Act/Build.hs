module Move.Act.Build
  ( Handle,
    new,
    build,
    toBuildConfig,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (EIO, toApp)
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

data Handle
  = Handle
  { collectHandle :: Collect.Handle,
    envHandle :: Env.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    fetchHandle :: Fetch.Handle,
    buildHandle :: Build.Handle
  }

new ::
  InitCompiler.Handle ->
  Fetch.Handle ->
  Collect.Handle ->
  Env.Handle ->
  Build.Handle ->
  Handle
new initCompilerHandle fetchHandle collectHandle envHandle buildHandle = do
  Handle {..}

build :: Handle -> Config -> App ()
build h cfg = do
  toApp $ setup h cfg
  target <- toApp $ Collect.getMainTarget (collectHandle h) $ targetName cfg
  mainModule <- toApp $ Env.getMainModule (envHandle h)
  Build.buildTarget (buildHandle h) mainModule (Main target)

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  LLVM.ensureSetupSanity cfg
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode (envHandle h) $ buildMode cfg
  Fetch.fetch (fetchHandle h) mainModule

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = outputKindList cfg,
      shouldSkipLink = shouldSkipLink cfg,
      shouldExecute = shouldExecute cfg,
      installDir = installDir cfg,
      executeArgs = args cfg
    }
