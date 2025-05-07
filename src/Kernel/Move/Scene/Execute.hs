module Kernel.Move.Scene.Execute
  ( Handle,
    new,
    execute,
  )
where

import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.External qualified as External
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Rule.Module (MainModule (MainModule))
import Kernel.Rule.Target
import Path

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let externalHandle = External.new loggerHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  let MainModule mainModule = Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  External.run (externalHandle h) (toFilePath outputPath) args
