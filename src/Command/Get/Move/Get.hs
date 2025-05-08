module Command.Get.Move.Get
  ( Handle,
    new,
    get,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Clean qualified as Clean
import Command.Common.Move.Fetch qualified as Fetch
import CommandParser.Rule.Config.Get
import CommandParser.Rule.Config.Remark qualified as Remark
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Error.Rule.EIO (EIO)
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.ModuleURL (ModuleURL (ModuleURL))
import Kernel.Move.Scene.Init.Global qualified as Global
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

get :: Handle -> Config -> EIO ()
get h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  Clean.clean (cleanHandle h)
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (ModuleURL $ moduleURLText cfg)
  h' <- liftIO $ Global.new (remarkCfg h) Nothing
  void $ Check.checkAll (Check.new h')
