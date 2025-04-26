module Move.Act.Create (create) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.New qualified as New
import Rule.Config.Create
import Rule.Module (moduleLocation)

create :: Config -> App ()
create cfg = do
  newModule <- toApp $ New.constructDefaultModule (moduleName cfg) (targetName cfg)
  hl <- InitLogger.new
  liftIO $ InitLogger.initializeLogger hl (remarkCfg cfg)
  hc <- InitCompiler.new
  liftIO $ InitCompiler.initializeCompilerWithModule hc newModule
  h <- New.new
  toApp $ New.createNewProject h (moduleName cfg) newModule
  hf <- Fetch.new
  toApp $ Fetch.insertCoreDependency hf
  toApp $ InitCompiler.initializeCompilerWithPath hc (moduleLocation newModule) (remarkCfg cfg)
  void Check.checkAll
