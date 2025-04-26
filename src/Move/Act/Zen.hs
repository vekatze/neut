module Move.Act.Zen (zen) where

import Data.Maybe
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Build (Axis (..), buildTarget)
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize
import Path.IO (resolveFile')
import Rule.Config.Zen
import Rule.Module (Module (moduleZenConfig), extractModule)
import Rule.OutputKind
import Rule.Target
import Rule.ZenConfig qualified as Z
import Prelude hiding (log)

zen :: Config -> App ()
zen cfg = do
  setup cfg
  path <- resolveFile' (filePathString cfg)
  mainModule <- Env.getMainModule
  buildTarget (fromConfig cfg) mainModule $
    Main $
      Zen path $
        Z.clangOption $
          moduleZenConfig (extractModule mainModule)

fromConfig :: Config -> Axis
fromConfig cfg =
  Axis
    { _outputKindList = [Object],
      _shouldSkipLink = False,
      _shouldExecute = True,
      _installDir = Nothing,
      _executeArgs = args cfg
    }

setup :: Config -> App ()
setup cfg = do
  Initialize.initializeCompiler (remarkCfg cfg)
  mainModule <- Env.getMainModule
  toApp $ Path.ensureNotInDependencyDir mainModule
  Env.setBuildMode $ buildMode cfg
  h <- Fetch.new
  toApp $ Fetch.fetch h mainModule
