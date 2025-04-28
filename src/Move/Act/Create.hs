module Move.Act.Create
  ( Handle,
    new,
    create,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Tag qualified as Tag
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.New qualified as New
import Rule.Config.Create
import Rule.Module (moduleLocation)

data Handle
  = Handle
  { initLoggerHandle :: InitLogger.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    newHandle :: New.Handle,
    fetchHandle :: Fetch.Handle,
    checkHandle :: Check.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Color.Handle -> Debug.Handle -> Locator.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle = do
  initLoggerHandle <- InitLogger.new envHandle colorHandle
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle
  newHandle <- New.new colorHandle
  fetchHandle <- Fetch.new envHandle gensymHandle colorHandle debugHandle
  checkHandle <- Check.new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle
  return $ Handle {..}

create :: Handle -> Config -> App ()
create h cfg = do
  newModule <- toApp $ New.constructDefaultModule (moduleName cfg) (targetName cfg)
  liftIO $ InitLogger.initializeLogger (initLoggerHandle h) (remarkCfg cfg)
  liftIO $ InitCompiler.initializeCompilerWithModule (initCompilerHandle h) newModule
  toApp $ New.createNewProject (newHandle h) (moduleName cfg) newModule
  toApp $ Fetch.insertCoreDependency (fetchHandle h)
  toApp $ InitCompiler.initializeCompilerWithPath (initCompilerHandle h) (moduleLocation newModule) (remarkCfg cfg)
  void $ Check.checkAll (checkHandle h)
