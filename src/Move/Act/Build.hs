module Move.Act.Build
  ( Handle,
    new,
    build,
    toBuildConfig,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.LLVM qualified as LLVM
import Move.Context.Path qualified as Path
import Move.Scene.Build qualified as Build
import Move.Scene.Collect qualified as Collect
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Rule.Config.Build
import Rule.Target
import Prelude hiding (log)

newtype Handle
  = Handle
  { baseHandle :: Base.Handle
  }

new ::
  Base.Handle ->
  Handle
new baseHandle = do
  Handle {..}

build :: Handle -> Config -> EIO ()
build h cfg = do
  setup h cfg
  let collectHandle = Collect.new (Base.envHandle (baseHandle h))
  let buildHandle = Build.new (toBuildConfig cfg) (baseHandle h)
  target <- Collect.getMainTarget collectHandle $ targetName cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Build.buildTarget buildHandle mainModule (Main target)

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  LLVM.ensureSetupSanity cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode (Base.envHandle (baseHandle h)) $ buildMode cfg
  let fetchHandle = Fetch.new (baseHandle h)
  Fetch.fetch fetchHandle mainModule

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = outputKindList cfg,
      shouldSkipLink = shouldSkipLink cfg,
      shouldExecute = shouldExecute cfg,
      installDir = installDir cfg,
      executeArgs = args cfg
    }
