module Act.Get (get) where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Entity.Config.Get
import Entity.Module
import Scene.Check qualified as Check
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInDependencyDir
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
  mainModule <- Env.getMainModule
  Initialize.initializeCompilerWithPath (moduleLocation mainModule) (remarkCfg cfg)
  void Check.checkAll
