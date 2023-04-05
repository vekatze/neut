module Act.Get (get) where

import Context.App
import Entity.Config.Get
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  Initialize.initializeCompiler (logCfg cfg) Nothing
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
