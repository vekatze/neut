module Command.Common.Move.Build.Execute
  ( Handle,
    new,
    execute,
  )
where

import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Module (MainModule (MainModule))
import Kernel.Common.Rule.Target
import Kernel.Move.Context.External qualified as External
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.Global.Path qualified as Path
import Kernel.Move.Scene.Init.Global qualified as Global
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let externalHandle = External.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  let MainModule mainModule = Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  External.run (externalHandle h) (toFilePath outputPath) args
