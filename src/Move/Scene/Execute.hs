module Move.Scene.Execute (execute) where

import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Rule.Target
import Path

execute :: MainTarget -> [String] -> App ()
execute target args = do
  mainModule <- Env.getMainModule
  outputPath <- Path.getExecutableOutputPath target mainModule
  External.run (toFilePath outputPath) args
