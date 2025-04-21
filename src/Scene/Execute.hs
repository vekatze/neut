module Scene.Execute (execute) where

import Context.App
import Context.Env qualified as Env
import Context.External qualified as External
import Context.Path qualified as Path
import Rule.Target
import Path

execute :: MainTarget -> [String] -> App ()
execute target args = do
  mainModule <- Env.getMainModule
  outputPath <- Path.getExecutableOutputPath target mainModule
  External.run (toFilePath outputPath) args
