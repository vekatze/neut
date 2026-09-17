module Command.Zen.Zen
  ( Handle,
    new,
    zen,
  )
where

import App.App (App)
import Command.Common.Build qualified as Build
import CommandParser.Config.Zen
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module (Module (moduleZenConfig), extractModule)
import Kernel.Common.OutputKind
import Kernel.Common.Target
import Path.IO (resolveFile')
import Prelude hiding (log)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

zen :: Handle -> Config -> App ()
zen h cfg = do
  let buildConfig = toBuildConfig cfg
  target <- getZenTarget cfg (globalHandle h)
  Build.buildMainTarget (Build.new buildConfig (globalHandle h)) target

getZenTarget :: Config -> Global.Handle -> App MainTarget
getZenTarget cfg globalHandle = do
  path <- resolveFile' (filePathString cfg)
  let mainModule = Env.getMainModule (Global.envHandle globalHandle)
  return $ Zen path $ moduleZenConfig (extractModule mainModule)

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = [Object],
      shouldSkipLink = False,
      shouldExecute = True,
      installDir = Nothing,
      executeArgs = args cfg
    }
