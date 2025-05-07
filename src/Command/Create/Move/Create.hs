module Command.Create.Move.Create
  ( Handle,
    new,
    create,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Fetch qualified as Fetch
import Command.Create.Move.Internal.Create qualified as Create
import CommandParser.Rule.Config.Create
import CommandParser.Rule.Config.Remark qualified as Remark
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Move.Scene.Module.Save qualified as ModuleSave
import Kernel.Rule.Module (moduleLocation)
import Logger.Rule.Handle qualified as Logger

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
