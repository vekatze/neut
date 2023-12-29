module Act.Format (format) where

import Context.App
import Context.Module qualified as Module
import Context.Parse qualified as Parse
import Entity.Config.Format
import Path.IO
import Scene.Format qualified as Format
import Scene.Initialize qualified as Initialize
import Scene.Write qualified as Write

format :: Config -> App ()
format cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Initialize.initializeForTarget
  path <- resolveFile' $ filePathString cfg
  source <- Module.sourceFromPath path
  Initialize.initializeForSource source
  content <- Format.format path
  if mustUpdateInPlace cfg
    then Write.write path content
    else Parse.printSourceFile content
