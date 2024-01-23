module Scene.Execute (execute) where

import Context.App
import Context.External qualified as External
import Context.Module qualified as Module
import Context.Path qualified as Path
import Entity.Target
import Path

execute :: ConcreteTarget -> [String] -> App ()
execute target args = do
  mainModule <- Module.getMainModule
  outputPath <- Path.getExecutableOutputPath target mainModule
  External.run (toFilePath outputPath) args
