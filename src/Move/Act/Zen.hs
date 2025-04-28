module Move.Act.Zen (zen) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Move.Context.App
import Move.Context.EIO (toApp)
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

zen :: Config -> App ()
zen cfg = do
  gensymHandle <- Gensym.new
  setup cfg gensymHandle
  path <- resolveFile' (filePathString cfg)
  envHandle <- Env.new
  buildHandle <- Build.new (toBuildConfig cfg) gensymHandle
  mainModule <- toApp $ Env.getMainModule envHandle
  Build.buildTarget buildHandle mainModule $
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

setup :: Config -> Gensym.Handle -> App ()
setup cfg gensymHandle = do
  hc <- InitCompiler.new gensymHandle
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  toApp $ Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode envHandle $ buildMode cfg
  h <- Fetch.new gensymHandle
  toApp $ Fetch.fetch h mainModule
