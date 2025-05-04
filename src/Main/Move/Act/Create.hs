module Main.Move.Act.Create
  ( Handle,
    new,
    create,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Scene.Check qualified as Check
import Main.Move.Scene.Create qualified as Create
import Main.Move.Scene.Fetch qualified as Fetch
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Save qualified as ModuleSave
import Main.Rule.Config.Create
import Main.Rule.Config.Remark qualified as Remark
import Main.Rule.Module (moduleLocation)

data Handle = Handle
  { createHandle :: Create.Handle,
    remarkCfg :: Remark.Config
  }

new :: Remark.Config -> Logger.Handle -> ModuleSave.Handle -> IO Handle
new remarkCfg loggerHandle moduleSaveHandle = do
  platformHandle <- Platform.new loggerHandle
  createHandle <- Create.new moduleSaveHandle loggerHandle platformHandle
  return $ Handle {..}

create :: Handle -> Config -> EIO ()
create h cfg = do
  newModule <- Create.constructDefaultModule (moduleName cfg) (targetName cfg)
  Create.createNewProject (createHandle h) (moduleName cfg) newModule
  h' <- liftIO $ Base.new (remarkCfg h) (Just $ moduleLocation newModule)
  Fetch.insertCoreDependency (Fetch.new h')
  void $ Check.checkAll (Check.new h')
