module Move.Act.Zen
  ( Handle,
    new,
    zen,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Move.Context.App
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Build qualified as Build
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Path.IO (resolveFile')
import Rule.Config.Zen
import Rule.Module (Module (moduleZenConfig), extractModule)
import Rule.OutputKind
import Rule.Target
import Rule.ZenConfig qualified as Z
import Prelude hiding (log)

data Handle
  = Handle
  { envHandle :: Env.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    fetchHandle :: Fetch.Handle,
    buildHandle :: Build.Handle
  }

new :: Config -> Gensym.Handle -> App Handle
new cfg gensymHandle = do
  envHandle <- Env.new
  initCompilerHandle <- InitCompiler.new gensymHandle
  fetchHandle <- Fetch.new gensymHandle
  buildHandle <- Build.new (toBuildConfig cfg) gensymHandle
  return $ Handle {..}

zen :: Handle -> Config -> App ()
zen h cfg = do
  toApp $ setup h cfg
  path <- resolveFile' (filePathString cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  Build.buildTarget (buildHandle h) mainModule $
    Main $
      Zen path $
        Z.clangOption $
          moduleZenConfig (extractModule mainModule)

toBuildConfig :: Config -> Build.Config
toBuildConfig cfg = do
  Build.Config
    { outputKindList = [Object],
      shouldSkipLink = False,
      shouldExecute = True,
      installDir = Nothing,
      executeArgs = args cfg
    }

setup :: Handle -> Config -> EIO ()
setup h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode (envHandle h) $ buildMode cfg
  Fetch.fetch (fetchHandle h) mainModule
