module Command.Zen.Move.Zen
  ( Handle,
    new,
    zen,
    toBuildConfig,
  )
where

import Command.Common.Move.Build qualified as Build
import Command.Common.Move.Fetch qualified as Fetch
import CommandParser.Rule.Config.Zen
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Rule.BuildMode qualified as BM
import Kernel.Rule.Module (Module (moduleZenConfig), extractModule)
import Kernel.Rule.OutputKind
import Kernel.Rule.Target
import Kernel.Rule.ZenConfig qualified as Z
import Path.IO (resolveFile')
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    fetchHandle :: Fetch.Handle,
    buildHandle :: Build.Handle
  }

new ::
  Base.Handle ->
  Config ->
  Handle
new baseHandle cfg = do
  let fetchHandle = Fetch.new baseHandle
  let buildHandle = Build.new (toBuildConfig cfg) baseHandle
  let envHandle = Base.envHandle baseHandle
  Handle {..}

zen :: Handle -> Config -> EIO ()
zen h cfg = do
  setup h cfg
  path <- resolveFile' (filePathString cfg)
  let mainModule = Env.getMainModule (envHandle h)
  Build.buildTarget (buildHandle h) mainModule $
    Main $
      Zen path $
        Z.clangOption $
          moduleZenConfig (extractModule mainModule)

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = [Object],
      shouldSkipLink = False,
      shouldExecute = True,
      installDir = Nothing,
      executeArgs = args cfg
    }

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  buildMode <- liftEither $ BM.fromString $ buildModeString cfg
  liftIO $ Env.setBuildMode (envHandle h) buildMode
  Fetch.fetch (fetchHandle h) mainModule
