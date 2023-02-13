module Act.Init
  ( initialize,
    Config (..),
    Context,
  )
where

import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Entity.Config.Init
import Scene.Initialize qualified as Initialize
import Scene.New qualified as New

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    Initialize.Context m,
    New.Context m,
    Module.Context m
  ) =>
  Context m

initialize :: Context m => Config -> m ()
initialize cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg)
  Initialize.initializeCompilerWithModule newModule (logCfg cfg) True
  New.createNewProject (moduleName cfg) newModule
