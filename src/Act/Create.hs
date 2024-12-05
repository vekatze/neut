module Act.Create (create) where

import Context.App
import Control.Monad
import Entity.Config.Create
import Entity.Module (moduleLocation)
import Scene.Check qualified as Check
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Scene.New qualified as New

create :: Config -> App ()
create cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg) (targetName cfg)
  Initialize.initializeLogger (remarkCfg cfg)
  Initialize.initializeCompilerWithModule newModule
  New.createNewProject (moduleName cfg) newModule
  Fetch.insertCoreDependency
  Initialize.initializeCompilerWithPath (moduleLocation newModule) (remarkCfg cfg)
  void Check.checkAll
