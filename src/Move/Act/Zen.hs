module Move.Act.Zen
  ( Handle,
    new,
    zen,
    toBuildConfig,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Build qualified as Build
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Path.IO (resolveFile')
import Rule.Config.Zen
import Rule.Module (Module (moduleZenConfig), extractModule)
import Rule.OutputKind
import Rule.Target
import Rule.ZenConfig qualified as Z
import Prelude hiding (log)

data Handle
  = Handle
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
  liftIO $ Env.setBuildMode (envHandle h) $ buildMode cfg
  Fetch.fetch (fetchHandle h) mainModule
