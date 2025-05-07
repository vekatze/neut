module Command.Common.Move.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Module (MainModule (MainModule))
import Kernel.Common.Rule.Target
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.Global.Path qualified as Path
import Kernel.Move.Context.ProcessRunner qualified as ProcessRunner
import Kernel.Move.Scene.Init.Global qualified as Global
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    processRunnerHandle :: ProcessRunner.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let processRunnerHandle = ProcessRunner.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  let MainModule mainModule = Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  ProcessRunner.run (processRunnerHandle h) (toFilePath outputPath) args
