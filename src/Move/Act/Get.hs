module Move.Act.Get
  ( Handle,
    new,
    get,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Check qualified as Check
import Move.Scene.Clean qualified as Clean
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Rule.Config.Get
import Rule.Config.Remark qualified as Remark
import Prelude hiding (log)

data Handle
  = Handle
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
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (moduleURL cfg)
  h' <- liftIO $ Base.new (remarkCfg h) Nothing
  void $ Check.checkAll (Check.new h')
