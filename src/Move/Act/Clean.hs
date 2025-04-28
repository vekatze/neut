module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clean qualified as Clean
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Clean
import Prelude hiding (log)

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    cleanHandle :: Clean.Handle
  }

new :: Gensym.Handle -> App Handle
new gensymHandle = do
  initCompilerHandle <- InitCompiler.new gensymHandle
  cleanHandle <- Clean.new gensymHandle
  return $ Handle {..}

clean :: Handle -> Config -> EIO ()
clean h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Clean.clean (cleanHandle h)
