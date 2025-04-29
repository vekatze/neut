module Move.Act.Create
  ( Handle,
    new,
    create,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.New qualified as New
import Rule.Config.Create
import Rule.Module (moduleLocation)

data Handle
  = Handle
  { initLoggerHandle :: InitLogger.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    newHandle :: New.Handle,
    fetchHandle :: Fetch.Handle,
    checkHandle :: Check.Handle
  }

new :: InitLogger.Handle -> InitCompiler.Handle -> New.Handle -> Fetch.Handle -> Check.Handle -> App Handle
new initLoggerHandle initCompilerHandle newHandle fetchHandle checkHandle = do
  return $ Handle {..}

create :: Handle -> Config -> App ()
create h cfg = do
  newModule <- toApp $ New.constructDefaultModule (moduleName cfg) (targetName cfg)
  liftIO $ InitLogger.initializeLogger (initLoggerHandle h) (remarkCfg cfg)
  liftIO $ InitCompiler.initializeCompilerWithModule (initCompilerHandle h) newModule
  toApp $ New.createNewProject (newHandle h) (moduleName cfg) newModule
  toApp $ Fetch.insertCoreDependency (fetchHandle h)
  toApp $ InitCompiler.initializeCompilerWithPath (initCompilerHandle h) (moduleLocation newModule) (remarkCfg cfg)
  void $ Check.checkAll (checkHandle h)
