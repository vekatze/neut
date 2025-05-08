module Command.Build.Move.Build
  ( Handle,
    new,
    build,
    toBuildConfig,
  )
where

import Command.Common.Move.Build qualified as Build
import Command.Common.Move.Fetch qualified as Fetch
import CommandParser.Rule.Config.Build
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Error.Rule.Error (Error)
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.BuildMode qualified as BM
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.OutputKind qualified as OK
import Kernel.Common.Rule.Target
import Kernel.Move.Scene.Init.Global qualified as Global
import Language.Common.Move.Raise (raiseError')
import Prelude hiding (log)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  Handle {..}

build :: Handle -> Config -> EIO ()
build h cfg = do
  setup h cfg
  buildConfig <- liftEither $ toBuildConfig cfg
  let buildHandle = Build.new buildConfig (globalHandle h)
  target <- getMainTarget h $ targetName cfg
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  Build.buildTarget buildHandle mainModule (Main target)

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  ensureSetupSanity cfg
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  Path.ensureNotInDependencyDir mainModule
  buildMode <- liftEither $ BM.fromString $ buildModeString cfg
  liftIO $ Env.setBuildMode (Global.envHandle (globalHandle h)) buildMode
  let fetchHandle = Fetch.new (globalHandle h)
  Fetch.fetch fetchHandle mainModule

toBuildConfig :: Config -> Either Error Build.Config
toBuildConfig cfg = do
  outputKindList <- mapM OK.fromText $ outputKindTextList cfg
  return $
    Build.Config
      { outputKindList = outputKindList,
        shouldSkipLink = shouldSkipLink cfg,
        shouldExecute = shouldExecute cfg,
        installDir = installDir cfg,
        executeArgs = args cfg
      }

getMainTarget :: Handle -> T.Text -> EIO MainTarget
getMainTarget h targetName = do
  let mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  case getTarget (extractModule mainModule) targetName of
    Just target ->
      return target
    Nothing ->
      raiseError' $ "No such target exists: " <> targetName

ensureSetupSanity :: Config -> EIO ()
ensureSetupSanity cfg = do
  outputKindList <- liftEither $ mapM OK.fromText $ outputKindTextList cfg
  let willBuildObjects = OK.Object `elem` outputKindList
  let willLink = not $ shouldSkipLink cfg
  when (not willBuildObjects && willLink) $
    raiseError' "`--skip-link` must be set explicitly when `--emit` does not contain `object`"
