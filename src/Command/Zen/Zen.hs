module Command.Zen.Zen
  ( Handle,
    new,
    zen,
    toBuildConfig,
  )
where

import App.App (App)
import Command.Common.Build qualified as Build
import Command.Common.Fetch qualified as Fetch
import CommandParser.Config.Zen
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Kernel.Common.BuildMode qualified as BM
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Module (Module (moduleZenConfig), extractModule)
import Kernel.Common.OutputKind
import Kernel.Common.Target
import Kernel.Common.ZenConfig qualified as Z
import Path.IO (resolveFile')
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    fetchHandle :: Fetch.Handle,
    buildHandle :: Build.Handle
  }

new ::
  Global.Handle ->
  Config ->
  Handle
new globalHandle cfg = do
  let fetchHandle = Fetch.new globalHandle
  let buildHandle = Build.new (toBuildConfig cfg) globalHandle
  let envHandle = Global.envHandle globalHandle
  Handle {..}

zen :: Handle -> Config -> App ()
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

setup :: Handle -> Config -> App ()
setup h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  buildMode <- liftEither $ BM.fromString $ buildModeString cfg
  liftIO $ Env.setBuildMode (envHandle h) buildMode
  Fetch.fetch (fetchHandle h) mainModule
