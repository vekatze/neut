module Move.Act.Create (create) where

import Move.Context.App
import Control.Monad
import Rule.Config.Create
import Rule.Module (moduleLocation)
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.New qualified as New

create :: Config -> App ()
create cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg) (targetName cfg)
  Initialize.initializeLogger (remarkCfg cfg)
  Initialize.initializeCompilerWithModule newModule
  New.createNewProject (moduleName cfg) newModule
  Fetch.insertCoreDependency
  Initialize.initializeCompilerWithPath (moduleLocation newModule) (remarkCfg cfg)
  void Check.checkAll
