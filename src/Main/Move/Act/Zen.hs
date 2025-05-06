module Main.Move.Act.Zen
  ( Handle,
    new,
    zen,
    toBuildConfig,
  )
where

import CommandParser.Rule.Config.Zen
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Error.Rule.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Build qualified as Build
import Main.Move.Scene.Fetch qualified as Fetch
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.BuildMode qualified as BM
import Main.Rule.Module (Module (moduleZenConfig), extractModule)
import Main.Rule.OutputKind
import Main.Rule.Target
import Main.Rule.ZenConfig qualified as Z
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
