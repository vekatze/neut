module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Context.App
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

new :: App Handle
new = do
  initCompilerHandle <- InitCompiler.new
  cleanHandle <- Clean.new
  return $ Handle {..}

clean :: Handle -> Config -> EIO ()
clean h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Clean.clean (cleanHandle h)
