module Move.Scene.Execute (execute) where

import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Path
import Rule.Module (MainModule (MainModule))
import Rule.Target

execute :: MainTarget -> [String] -> App ()
execute target args = do
  MainModule mainModule <- Env.getMainModule
  h <- Path.new
  outputPath <- toApp $ Path.getExecutableOutputPath h target mainModule
  h' <- External.new
  toApp $ External.run h' (toFilePath outputPath) args
