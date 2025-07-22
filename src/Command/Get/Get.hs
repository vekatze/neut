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
import CommandParser.Config.Remark qualified as Remark
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.ModuleURL (ModuleURL (ModuleURL))
import Prelude hiding (log)

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    cleanHandle :: Clean.Handle,
    checkHandle :: Check.Handle,
    remarkCfg :: Remark.Config
  }

new ::
  Global.Handle ->
  Remark.Config ->
  IO Handle
new globalHandle remarkCfg = do
  let envHandle = Global.envHandle globalHandle
  let fetchHandle = Fetch.new globalHandle
  let checkHandle = Check.new globalHandle
  cleanHandle <- Clean.new globalHandle
  return $ Handle {..}

get :: Handle -> Config -> App ()
get h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  Clean.clean (cleanHandle h)
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (ModuleURL $ moduleURLText cfg)
  h' <- liftIO $ Global.new (remarkCfg h) Nothing
  void $ Check.checkAll (Check.new h')
