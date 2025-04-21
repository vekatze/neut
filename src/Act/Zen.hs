module Act.Zen (zen) where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Data.Maybe
import Rule.Config.Zen
import Rule.Module (Module (moduleZenConfig))
import Rule.OutputKind
import Rule.Target
import Rule.ZenConfig qualified as Z
import Path.IO (resolveFile')
import Scene.Build (Axis (..), buildTarget)
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
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
          moduleZenConfig mainModule

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
  Path.ensureNotInDependencyDir
  Env.setBuildMode $ buildMode cfg
  Env.getMainModule >>= Fetch.fetch
