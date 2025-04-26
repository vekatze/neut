module Move.Act.Clean (clean) where

import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Clean qualified as Clean
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Clean
import Prelude hiding (log)

clean :: Config -> App ()
clean cfg = do
  hc <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  h <- Clean.new
  toApp $ Clean.clean h
