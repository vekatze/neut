module Main.Move.Act.Build
  ( Handle,
    new,
    build,
    toBuildConfig,
  )
where

import CommandParser.Rule.Config.Build
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Error.Rule.Error (Error)
import Language.Common.Move.Raise (raiseError')
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Build qualified as Build
import Main.Move.Scene.Fetch qualified as Fetch
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.BuildMode qualified as BM
import Main.Rule.Module
import Main.Rule.OutputKind qualified as OK
import Main.Rule.Target
import Prelude hiding (log)

newtype Handle = Handle
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
  buildConfig <- liftEither $ toBuildConfig cfg
  let buildHandle = Build.new buildConfig (baseHandle h)
  target <- getMainTarget h $ targetName cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Build.buildTarget buildHandle mainModule (Main target)

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  ensureSetupSanity cfg
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  Path.ensureNotInDependencyDir mainModule
  buildMode <- liftEither $ BM.fromString $ buildModeString cfg
  liftIO $ Env.setBuildMode (Base.envHandle (baseHandle h)) buildMode
  let fetchHandle = Fetch.new (baseHandle h)
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
  let mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
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
