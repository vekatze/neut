module Command.Create.Create
  ( Handle,
    new,
    create,
  )
where

import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.Common.SaveModule qualified as SaveModule
import Command.Create.Internal.MoveCreate qualified as Create
import CommandParser.Config.Create
import CommandParser.Config.Remark qualified as Remark
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module (moduleLocation)
import Logger.Handle qualified as Logger

data Handle = Handle
  { createHandle :: Create.Handle,
    remarkCfg :: Remark.Config
  }

new :: Remark.Config -> Logger.Handle -> SaveModule.Handle -> IO Handle
new remarkCfg loggerHandle saveModuleHandle = do
  platformHandle <- Platform.new loggerHandle
  createHandle <- Create.new saveModuleHandle loggerHandle platformHandle
  return $ Handle {..}

create :: Handle -> Config -> EIO ()
create h cfg = do
  newModule <- Create.constructDefaultModule (moduleName cfg) (targetName cfg)
  Create.createNewProject (createHandle h) (moduleName cfg) newModule
  h' <- liftIO $ Global.new (remarkCfg h) (Just $ moduleLocation newModule)
  Fetch.insertCoreDependency (Fetch.new h')
  void $ Check.checkAll (Check.new h')
