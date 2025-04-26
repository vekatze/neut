module Move.Act.Clean (clean) where

import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Clean qualified as Clean
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Rule.Config.Clean
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  hm <- ModuleReflect.new
  Initialize.initializeCompiler hm (remarkCfg cfg)
  h <- Clean.new
  toApp $ Clean.clean h
