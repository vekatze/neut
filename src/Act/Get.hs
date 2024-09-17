module Act.Get (get) where

import Context.App
import Context.Path qualified as Path
import Entity.Config.Get
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  Path.ensureNotInLibDir
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
