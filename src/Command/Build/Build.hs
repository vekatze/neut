module Command.Build.Build
  ( Handle,
    new,
    build,
  )
where

import App.App (App)
import App.Run (raiseError')
import Command.Common.Build qualified as Build
import CommandParser.Config.Build
import Control.Monad
import Control.Monad.Except (liftEither)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.OutputKind qualified as OK
import Prelude hiding (log)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

build :: Handle -> Config -> App ()
build h cfg = do
  buildConfig <- toBuildConfig cfg
  target <- Build.getMainTarget (targetName cfg) (globalHandle h)
  Build.buildMainTarget (Build.new buildConfig (globalHandle h)) target

toBuildConfig :: Config -> App Build.Config
toBuildConfig cfg = do
  outputKindList <- liftEither $ mapM OK.fromText $ outputKindTextList cfg
  ensureSetupSanity outputKindList (shouldSkipLink cfg)
  return $
    Build.Config
      { outputKindList = outputKindList,
        shouldSkipLink = shouldSkipLink cfg,
        shouldExecute = shouldExecute cfg,
        installDir = installDir cfg,
        executeArgs = args cfg
      }

ensureSetupSanity :: [OK.OutputKind] -> Bool -> App ()
ensureSetupSanity outputKindList shouldSkipLink' = do
  let willBuildObjects = OK.Object `elem` outputKindList
  let willLink = not shouldSkipLink'
  when (not willBuildObjects && willLink) $
    raiseError' "`--skip-link` must be set explicitly when `--emit` does not contain `object`"
