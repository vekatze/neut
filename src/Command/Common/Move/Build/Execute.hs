module Command.Common.Move.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import Error.Rule.EIO (EIO)
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Move.RunProcess qualified as RunProcess
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Module (MainModule (MainModule))
import Kernel.Common.Rule.Target
import Kernel.Move.Scene.Init.Global qualified as Global
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  let MainModule mainModule = Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  RunProcess.run (runProcessHandle h) (toFilePath outputPath) args
