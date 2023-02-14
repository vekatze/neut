module Scene.Execute
  ( Context,
    execute,
  )
where

import Context.Env qualified as Env
import Context.External qualified as External
import Entity.Target
import Path
import Scene.Initialize qualified as Initialize
import Scene.Module.Path

class
  ( Env.Context m,
    Initialize.Context m,
    External.Context m
  ) =>
  Context m

execute :: Context m => Target -> m ()
execute target = do
  mainModule <- Env.getMainModule
  outputPath <- getExecutableOutputPath target mainModule
  External.run (toFilePath outputPath) []
