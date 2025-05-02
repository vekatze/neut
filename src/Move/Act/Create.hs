module Move.Act.Create
  ( Handle,
    new,
    create,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Console.Report qualified as Report
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Create qualified as Create
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Module.Save qualified as ModuleSave
import Rule.Config.Create
import Rule.Config.Remark qualified as Remark
import Rule.Module (moduleLocation)

data Handle
  = Handle
  { newHandle :: Create.Handle,
    remarkCfg :: Remark.Config
  }

new :: Remark.Config -> Report.Handle -> Debug.Handle -> ModuleSave.Handle -> IO Handle
new remarkCfg reportHandle debugHandle moduleSaveHandle = do
  envHandle <- Env.new reportHandle debugHandle (Remark.enableSilentMode remarkCfg) Nothing
  newHandle <- Create.new moduleSaveHandle reportHandle envHandle
  return $ Handle {..}

create :: Handle -> Config -> EIO ()
create h cfg = do
  newModule <- Create.constructDefaultModule (moduleName cfg) (targetName cfg)
  Create.createNewProject (newHandle h) (moduleName cfg) newModule
  h' <- liftIO $ Base.new (remarkCfg h) (Just $ moduleLocation newModule)
  Fetch.insertCoreDependency (Fetch.new h')
  void $ Check.checkAll (Check.new h')
