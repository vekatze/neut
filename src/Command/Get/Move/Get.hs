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
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Common.Rule.ModuleURL (ModuleURL (ModuleURL))
import Prelude hiding (log)

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    cleanHandle :: Clean.Handle,
    checkHandle :: Check.Handle,
    remarkCfg :: Remark.Config
  }

new ::
  Base.Handle ->
  Remark.Config ->
  IO Handle
new baseHandle remarkCfg = do
  let envHandle = Base.envHandle baseHandle
  let fetchHandle = Fetch.new baseHandle
  let checkHandle = Check.new baseHandle
  cleanHandle <- Clean.new baseHandle
  return $ Handle {..}

get :: Handle -> Config -> EIO ()
get h cfg = do
  let mainModule = Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  Clean.clean (cleanHandle h)
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (ModuleURL $ moduleURLText cfg)
  h' <- liftIO $ Base.new (remarkCfg h) Nothing
  void $ Check.checkAll (Check.new h')
