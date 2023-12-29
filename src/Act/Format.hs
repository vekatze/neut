module Act.Format (format) where

import Context.App
import Entity.Config.Format
import Scene.Format qualified as Format
import Scene.Initialize qualified as Initialize
import Scene.Write qualified as Write

format :: Config -> App ()
format cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Initialize.initializeForTarget
  let path = filePathString cfg
  Format.format path >>= Write.write path
