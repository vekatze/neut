module Move.Scene.Execute
  ( Handle,
    new,
    execute,
  )
where

import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Path
import Rule.Module (MainModule (MainModule))
import Rule.Target

data Handle
  = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle
  }

new :: Env.Handle -> App Handle
new envHandle = do
  pathHandle <- Path.new envHandle
  externalHandle <- External.new
  return $ Handle {..}

execute :: Handle -> MainTarget -> [String] -> EIO ()
execute h target args = do
  MainModule mainModule <- Env.getMainModule (envHandle h)
  outputPath <- Path.getExecutableOutputPath (pathHandle h) target mainModule
  External.run (externalHandle h) (toFilePath outputPath) args
