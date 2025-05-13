module Command.Create.Move.Create
  ( Handle,
    new,
    create,
  )
where

import CommandParser.Rule.Config.Create
import CommandParser.Rule.Config.Remark qualified as Remark
import Error.Rule.EIO (EIO)
import Logger.Rule.Handle qualified as Logger
import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Fetch qualified as Fetch
import Command.Common.Move.SaveModule qualified as SaveModule
import Command.Create.Move.Internal.Create qualified as Create
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Module (moduleLocation)

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
