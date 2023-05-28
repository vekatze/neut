module Act.Add (add) where

import Context.App
import Context.Path qualified as Path
import Entity.Config.Add
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

add :: Config -> App ()
add cfg = do
  Initialize.initializeCompiler (remarkCfg cfg) Nothing
  Path.ensureNotInLibDir
  Fetch.insertDependency (moduleAliasText cfg) (moduleURL cfg)
