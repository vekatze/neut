module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Tag qualified as Tag
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

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> Tag.Handle -> App Handle
new envHandle gensymHandle locatorHandle tagHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle
  cleanHandle <- Clean.new envHandle gensymHandle locatorHandle tagHandle
  return $ Handle {..}

clean :: Handle -> Config -> EIO ()
clean h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Clean.clean (cleanHandle h)
