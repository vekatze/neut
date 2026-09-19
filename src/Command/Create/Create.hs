module Command.Create.Create
  ( Handle,
    new,
    create,
  )
where

import App.App (App)
import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.Common.SaveModule qualified as SaveModule
import Command.Create.Internal qualified as Create
import CommandParser.Config.Create
import Console.Handle qualified as Console
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.LocalArchive qualified as LocalArchive
import Kernel.Common.Module (moduleLocation)
import Kernel.Common.Platform qualified as P
import Logger.Handle qualified as Logger

data Handle = Handle
  { createHandle :: Create.Handle,
    consoleHandle :: Console.Handle,
    loggerHandle :: Logger.Handle,
    localArchiveMap :: LocalArchive.LocalArchiveMap
  }

new :: Console.Handle -> Logger.Handle -> SaveModule.Handle -> LocalArchive.LocalArchiveMap -> App Handle
new consoleHandle loggerHandle saveModuleHandle localArchiveMap = do
  platformHandle <- Platform.new loggerHandle P.SelectHost
  createHandle <- liftIO $ Create.new saveModuleHandle loggerHandle platformHandle
  return $ Handle {..}

create :: Handle -> Config -> App ()
create h cfg = do
  newModule <- Create.constructDefaultModule (moduleName cfg) (targetName cfg)
  coreLocation <- Fetch.getCoreLocation
  Create.createNewProject (createHandle h) (moduleName cfg) newModule
  h' <- Global.new (consoleHandle h) (loggerHandle h) (localArchiveMap h) (Just $ moduleLocation newModule) Nothing
  Fetch.insertCoreDependency (Fetch.new h') coreLocation
  h'' <- Global.new (consoleHandle h) (loggerHandle h) (localArchiveMap h) (Just $ moduleLocation newModule) Nothing
  void $ Check.checkAllOrFail (Check.new h'') Nothing
