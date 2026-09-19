module Command.Get.Get
  ( Handle,
    new,
    get,
  )
where

import App.App (App)
import Command.Common.Check qualified as Check
import Command.Common.Clean qualified as Clean
import Command.Common.Fetch qualified as Fetch
import CommandParser.Config.Get
import Console.Handle qualified as Console
import Control.Monad
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.LocalArchive qualified as LocalArchive
import Kernel.Common.ModuleURL (ModuleURL (ModuleURL))
import Logger.Handle qualified as Logger
import Prelude hiding (log)

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    cleanHandle :: Clean.Handle,
    checkHandle :: Check.Handle,
    consoleHandle :: Console.Handle,
    loggerHandle :: Logger.Handle,
    localArchiveMap :: LocalArchive.LocalArchiveMap
  }

new ::
  Global.Handle ->
  IO Handle
new globalHandle = do
  let envHandle = Global.envHandle globalHandle
  let fetchHandle = Fetch.new globalHandle
  let checkHandle = Check.new globalHandle
  cleanHandle <- Clean.new globalHandle
  let consoleHandle = Global.consoleHandle globalHandle
  let loggerHandle = Global.loggerHandle globalHandle
  let localArchiveMap = Global.localArchiveMap globalHandle
  return $ Handle {..}

get :: Handle -> Config -> App ()
get h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  Clean.clean (cleanHandle h)
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (ModuleURL $ moduleURLText cfg)
  h' <- Global.new (consoleHandle h) (loggerHandle h) (localArchiveMap h) Nothing Nothing
  void $ Check.checkAllOrFail (Check.new h') Nothing
