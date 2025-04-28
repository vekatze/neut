module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
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

new :: Env.Handle -> Gensym.Handle -> Color.Handle -> Report.Handle -> Debug.Handle -> Locator.Handle -> KeyArg.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle colorHandle reportHandle debugHandle locatorHandle keyArgHandle tagHandle antecedentHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle reportHandle debugHandle
  cleanHandle <- Clean.new envHandle gensymHandle debugHandle locatorHandle keyArgHandle tagHandle antecedentHandle
  return $ Handle {..}

clean :: Handle -> Config -> EIO ()
clean h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  Clean.clean (cleanHandle h)
