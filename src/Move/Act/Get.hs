module Move.Act.Get (get) where

import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Control.Monad
import Rule.Config.Get
import Rule.Module
import Move.Scene.Check qualified as Check
import Move.Scene.Clean qualified as Clean
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInDependencyDir
  Clean.clean
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
  mainModule <- Env.getMainModule
  Initialize.initializeCompilerWithPath (moduleLocation mainModule) (remarkCfg cfg)
  void Check.checkAll
