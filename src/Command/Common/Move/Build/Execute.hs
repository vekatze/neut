module Command.Common.Move.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Move.RunProcess qualified as RunProcess
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Target
import Path

data Handle = Handle
  { pathHandle :: Path.Handle,
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target
  _ <- liftIO $ RunProcess.runProcess (runProcessHandle h) (toFilePath outputPath) args
  return ()
