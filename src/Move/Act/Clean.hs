module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Context.EIO (EIO)
import Move.Scene.Clean qualified as Clean
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Clean
import Prelude hiding (log)

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    cleanHandle :: Clean.Handle
  }

new :: InitCompiler.Handle -> Clean.Handle -> Handle
new initCompilerHandle cleanHandle = do
  Handle {..}

clean :: Handle -> Config -> EIO ()
clean h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Clean.clean (cleanHandle h)
