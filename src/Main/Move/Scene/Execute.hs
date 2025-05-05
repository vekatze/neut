module Main.Move.Scene.Execute
  ( Handle,
    new,
    execute,
  )
where

import Error.Rule.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.External qualified as External
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Module (MainModule (MainModule))
import Main.Rule.Target
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
