module Move.Act.Format (format) where

import Move.Context.App
import Move.Context.Parse (ensureExistence', readTextFile)
import Move.Context.Parse qualified as Parse
import Rule.Config.Format
import Path.IO
import Move.Scene.Format qualified as Format
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Write qualified as Write

format :: Config -> App ()
format cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Initialize.initializeForTarget
  path <- resolveFile' $ filePathString cfg
  ensureExistence' path Nothing
  content <- readTextFile path
  content' <- Format.format (shouldMinimizeImports cfg) (inputFileType cfg) path content
  if mustUpdateInPlace cfg
    then Write.write path content'
    else Parse.printTextFile content'
