module Act.Create (create) where

import Context.App
import Entity.Config.Create
import Scene.Initialize qualified as Initialize
import Scene.New qualified as New

create :: Config -> App ()
create cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg)
  Initialize.initializeCompilerWithModule newModule (logCfg cfg) True Nothing
  New.createNewProject (moduleName cfg) newModule
