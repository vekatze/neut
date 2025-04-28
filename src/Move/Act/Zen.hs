module Move.Act.Zen (zen) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Build (Axis (..), buildTarget)
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
  g <- Gensym.new
  setup cfg
  path <- resolveFile' (filePathString cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  buildTarget (fromConfig g cfg) mainModule $
    Main $
      Zen path $
        Z.clangOption $
          moduleZenConfig (extractModule mainModule)

fromConfig :: Gensym.Handle -> Config -> Axis
fromConfig gensymHandle cfg =
  Axis
    { gensymHandle = gensymHandle,
      _outputKindList = [Object],
      _shouldSkipLink = False,
      _shouldExecute = True,
      _installDir = Nothing,
      _executeArgs = args cfg
    }

setup :: Config -> App ()
setup cfg = do
  hc <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  toApp $ Path.ensureNotInDependencyDir mainModule
  liftIO $ Env.setBuildMode envHandle $ buildMode cfg
  h <- Fetch.new
  toApp $ Fetch.fetch h mainModule
