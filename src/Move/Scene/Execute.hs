module Move.Scene.Execute
  ( Handle,
    new,
    execute,
  )
where

import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Move.Scene.Init.Base qualified as Base
import Path
import Rule.Module (MainModule (MainModule))
import Rule.Target

data Handle
  = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let externalHandle = External.new debugHandle
  Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  let MainModule mainModule = Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  External.run (externalHandle h) (toFilePath outputPath) args
