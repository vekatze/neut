module Act.Create (create) where

import Context.App
import Entity.Config.Create
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Scene.New qualified as New

create :: Config -> App ()
create cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg)
  Initialize.initializeCompilerWithModule newModule (remarkCfg cfg) Nothing
  New.createNewProject (moduleName cfg) newModule
  Fetch.insertCoreDependency
