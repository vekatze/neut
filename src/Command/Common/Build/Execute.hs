module Command.Common.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import App.App (App)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Target
import Path

data Handle = Handle
  { pathHandle :: Path.Handle,
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> App ()
execute h target args = do
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  _ <- liftIO $ RunProcess.runProcess (runProcessHandle h) (toFilePath outputPath) args
  return ()
