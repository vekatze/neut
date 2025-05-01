module Move.Act.Create
  ( Handle,
    new,
    create,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Move.Scene.New qualified as New
import Rule.Config.Create
import Rule.Config.Remark qualified as Remark
import Rule.Module (moduleLocation)

data Handle
  = Handle
  { newHandle :: New.Handle,
    remarkCfg :: Remark.Config
  }

new :: New.Handle -> Remark.Config -> Handle
new newHandle remarkCfg = do
  Handle {..}

create :: Handle -> Config -> EIO ()
create h cfg = do
  newModule <- New.constructDefaultModule (moduleName cfg) (targetName cfg)
  New.createNewProject (newHandle h) (moduleName cfg) newModule
  h' <- liftIO $ Base.new (remarkCfg h) (Just $ moduleLocation newModule)
  Fetch.insertCoreDependency (Fetch.new h')
  void $ Check.checkAll (Check.new h')
