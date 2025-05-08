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
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.BuildMode qualified as BM
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Module (Module (moduleZenConfig), extractModule)
import Kernel.Common.Rule.OutputKind
import Kernel.Common.Rule.Target
import Kernel.Common.Rule.ZenConfig qualified as Z
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
